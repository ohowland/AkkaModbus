package CGC.Modbus

import scala.math.pow
import akka.actor.{ Actor, ActorLogging, Props}

object MessageFactory {
  def props(modbusMap: List[ModbusTypes.ModbusRegsiter]): Props = Props(new MessageFactory(modbusMap))

  // Message Interface for the Factory
  case class ReqCreateMessageTemplate(groupName: String)
  case class RespCreateMessageTemplate(groupName: String, template: List[ModbusMessageTemplate])

  /**
    * Defines the data required for a modbus read multiple holding registers command.
    *
    * @param startAddress      is the start address for the modbus block read (inclusive)
    * @param numberOfRegisters is the number of registers to read, including the first.
    */
  case class ReadSpecification(startAddress: Int, numberOfRegisters: Int)

  /**
    * Defines the a template that contains the data required to create a modbus polling request, and decode the
    * returned data.
    *
    * @param specification data required to fulfill a Modbus read multiple holding registers command.
    * @param registers     a list of Modbus register classes which defines all holding registers in this message's domain.
    * @param endianness    the endianess of the message.
    */
  case class ModbusMessageTemplate(specification: ReadSpecification,
                                   registers: List[ModbusTypes.ModbusRegsiter],
                                   endianness: String) {
    /**
      * Returns map of register name to register value as Map[String, Double].
      *
      * The decode method operates on the List[Int] returned by a read holding registers modbus call.
      * Messages created from a template are then decoded by the same template.
      *
      * @param response : A list of holding register values returned by a modbus poll
      */
    def decode(response: List[Int]): Map[String, Double] = {

      /**
        * Returns a single Double type from a list of two byte values.
        *
        * The 2 byte 'words' are the Modbus standard holding register size. Words must be processed into
        * the correct type once read from the target device.
        *
        * @param words      List of 2 byte holding register values
        * @param endianness The order in which the list is interpreted. Two acceptible values: "big" and "little".
        */
      def buildDouble(words: List[Int], endianness: String): Double = {

        /**
          * Aggregates and returns the value of a Double by recusivly reading through a list of 2 byte words.
          *
          * @param words         is a list of 2 byte holding register values
          * @param word_position indexes current 2 bytes into their position in the double.
          * @param accumulator   accumulates the final value while recursing.
          * @return Double composed from a list of 2 byte words.
          */
        def buildDoubleIter(words: List[Int], word_position: Int, accumulator: Double): Double = {
          if (word_position >= words.size) accumulator
          else buildDoubleIter(words,
            word_position + 1,
            words(word_position).toDouble * pow(2, 16 * word_position) + accumulator)
        }

        val orderedWords = endianness match {
          case "big" => words.reverse
          case "little" => words
        }
        buildDoubleIter(orderedWords, 0, 0.0)
      }

      /**
        * Returns a subset list of the response parameter based on index and datatype size.
        *
        * @param response a list of 2 byte holding registers returned from a modbus read holding registers command.
        * @param datatype the target datatype
        * @param index    the start index in the response for the current
        * @return subset integer list of response
        */
      def sliceHoldingRegisters(response: List[Int], datatype: ModbusTypes.ModbusDatatype, index: Int): List[Int] =
        response.slice(index, index + modbusDatatypeWords(datatype))

      // the decode method takes each register, finds its location in the response list, and converts the
      // correct number of sequential words into a double
      (for {
        register <- registers
        value = sliceHoldingRegisters(response, register.datatype, register.address - registers.head.address)
      } yield register.name -> buildDouble(value, endianness)).toMap
    }
  }

  /**
    * Returns a ModbusMessageTemplate class configured with required data to complete a read holding registers
    * request.
    *
    * @param modbusMap is a list of ModbusRegisters which define communcation with a device.
    * @param groupName is used to select registers from the modbusMap based on the field ModbusRegister.group
    */
  def createModbusMessageTemplates(modbusMap: List[ModbusTypes.ModbusRegsiter],
                                   groupName: String,
                                   endianness: String): List[ModbusMessageTemplate] = {
    /**
      * Returns a set of all blocks in the Modbus map for a given groupName.
      *
      * @param modbusMap is a list of ModbusRegisters which define communcation with a device.
      * @param groupName is used to select registers from the modbusMap based on the field ModbusRegister.group.
      */
    def findBlocks(modbusMap: List[ModbusTypes.ModbusRegsiter], groupName: String): Set[Int] =
      modbusMap.filter(_.group == groupName).map(_.block).toSet

    /**
      * Returns a bucketed list of modbus registers based on register block number.
      *
      * Example:
      * List(List(register1.block == 1, register2.block == 1),
      * List(register13.block == 2),
      * List(register22.block == 6))
      *
      * @param modbusMap is a list of ModbusRegisters which define communcation with a device.
      * @param blocks    is a set of all block numbers contained in the modbusMap.
      */
    def bucketRegisterListByBlock(modbusMap: List[ModbusTypes.ModbusRegsiter],
                                  blocks: Set[Int]): List[List[ModbusTypes.ModbusRegsiter]] = {
      (for {
        block <- blocks
        registerList = modbusMap.filter(_.block == block)
      } yield registerList).toList
    }

    /**
      * Returns a ReadSpecification for the registerlist
      *
      * @param registerList
      * @return
      */
    def buildReadSpecification(registerList: List[ModbusTypes.ModbusRegsiter]): ReadSpecification = {
      val startAddress: Int = registerList.map(_.address).min
      val lastAddress: Int = registerList.map(_.address).max
      val lastAddressType: ModbusTypes.ModbusDatatype = modbusMap.filter(_.address == lastAddress).map(_.datatype).head
      val numberOfRegisters: Int = lastAddress + modbusDatatypeWords(lastAddressType) - startAddress
      ReadSpecification(startAddress, numberOfRegisters)
    }

    for {
      registerBlock <- bucketRegisterListByBlock(modbusMap, findBlocks(modbusMap, groupName))
    } yield ModbusMessageTemplate(buildReadSpecification(registerBlock), registerBlock, endianness)
  }

  /** converts the datatype to a word value (2 bytes), used in defining transaction lengths.
    *
    * @param datatype
    * @return
    */
  private def modbusDatatypeWords(datatype: ModbusTypes.ModbusDatatype): Int = datatype match {
    case ModbusTypes.U16 => 1
    case ModbusTypes.U32 => 2
    case ModbusTypes.I16 => 1
    case ModbusTypes.I32 => 2
    case ModbusTypes.F32 => 2
    case ModbusTypes.F64 => 4
  }

}

class MessageFactory(modbusMap: List[ModbusTypes.ModbusRegsiter]) extends Actor
  with ActorLogging {

  import MessageFactory._

  val endianness = "big"

  def receive: Receive = {
    case ReqCreateMessageTemplate(groupName) =>
      sender() ! RespCreateMessageTemplate(groupName, createModbusMessageTemplates(modbusMap, groupName, endianness))
  }
}
