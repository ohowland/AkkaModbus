package CGC.AssetCommunications

import scala.math.pow

object MessageFactory {

  case class ReadSpecification(startAddress: Int, numberOfRegisters: Int)

  def createModbusMessageTemplates(modbusMap: List[Comm.ModbusRegsiter],
                                   groupName: String): Set[ModbusMessageTemplate] = {
    /** Blocks are a consecutive set of registers with size no larger than the maximum size defined in the
      * Modbus specification. The size is not enforced here, we assume the blocks in the specifcation file have
      * been sized correctly.
      */
    val blocks = modbusMap.filter(_.group == groupName).map(_.block).toSet

    val blockRegisterLists = for {
      block <- blocks
      registerList = modbusMap.filter(_.block == block)
    } yield registerList

    for {
      registerList: List[Comm.ModbusRegsiter] <- blockRegisterLists
      startAddress: Int = registerList.map(_.address).min
      lastAddress: Int = registerList.map(_.address).max
      lastAddressType: Comm.ModbusDatatype = modbusMap.filter(_.address == lastAddress).map(_.datatype).head
      numberOfRegisters: Int = lastAddress + modbusDatatypeWords(lastAddressType) - startAddress
    } yield ModbusMessageTemplate(ReadSpecification(startAddress, numberOfRegisters), registerList, "big")
  }

  case class ModbusMessageTemplate(rs: ReadSpecification,
                                   registers: List[Comm.ModbusRegsiter],
                                   endianness: String) {

    def decode(response: List[Int]): Map[String, Double] = {

      def buildDouble(words: List[Int], endianness: String): Double = {

        def buildDoubleIter(words: List[Int], word_position: Int, aggregate: Double): Double = {
          if (word_position >= words.size) aggregate
          else buildDoubleIter(words, word_position + 1, words(word_position).toDouble * pow(2, 16 * word_position) + aggregate)
        }

        val orderedWords = endianness match {
          case "big" => words.reverse
          case "little" => words
        }
        buildDoubleIter(orderedWords, 0, 0.0)
      }

      val startingAddress: Int = registers.head.address
      (for {
        register <- registers
        index = register.address - startingAddress
        value: List[Int] = register.datatype match {
          case Comm.U16 => List(response(index))
          case Comm.I16 => List(response(index))
          case Comm.U32 => List(response(index), response(index + 1))
          case Comm.I32 => List(response(index), response(index + 1))
          case Comm.F32 => List(response(index), response(index + 1))
          case Comm.F64 => List(response(index), response(index + 1), response(index + 2), response(index + 3))
        }
      } yield register.name -> buildDouble(value, endianness)).toMap
    }

    def build(requestId: Long): Comm.ReqReadHoldingRegisters = {
      Comm.ReqReadHoldingRegisters(
        requestId,
        rs.startAddress,
        rs.numberOfRegisters)
    }
  }

  def modbusDatatypeWords(datatype: Comm.ModbusDatatype): Int = datatype match {
    case Comm.U16 => 1
    case Comm.U32 => 2
    case Comm.I16 => 1
    case Comm.I32 => 2
    case Comm.F32 => 2
    case Comm.F64 => 4
  }
}
