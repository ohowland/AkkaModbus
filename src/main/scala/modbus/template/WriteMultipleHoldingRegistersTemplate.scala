/**
  * TEMPLATES TURN MAP[STRING, DOUBLE] -> LIST[INT]
  * AND LIST[INT] -> MAP[STRING, DOUBLE]
  *
  * NO PDUS
  */
package modbus.template

import scala.math.pow

class WriteMultipleHoldingRegistersTemplate(specification: Template.BlockSpecification,
                                            registers: List[Modbus.Register],
                                            endianness: String) extends Template {

  // Returns a a PDU of holding register values that can be packaged into a request.
  override def encode(request: Map[String, Double]): List[Int] = {
    /**
      * Returns a list of two byte values which represent a decomposed Double.
      *
      * The 2 byte 'words' are the Modbus standard holding register size. Words must be processed into
      * the correct type once read from the target device.
      *
      * @param value      A single Double type to be decomposed into a list of two byte values
      */
    def decomposeDouble(value: Double, register: Modbus.Register): List[Int] = {
      def decomposeDoubleIter(value: BigInt, acc: List[Int]): List[Int] = {
        if (acc.size >= register.datatype.nWords) acc
        else {
          val mask = BigDecimal(0xFFFF * pow(2, 16 * acc.size)).toBigInt
          val nextWord = ((value & mask) >> (16 * acc.size)).toInt
          decomposeDoubleIter(value, nextWord :: acc)}
      }
      decomposeDoubleIter(BigDecimal(unscale(value, register)).toBigInt, List.empty)
    }

    //TODO: I'm brain dead. What are we doing with the List[Int] package that is being created? It needs to find its way into the RequestWriteMultipleHoldingRegisters class object.
  }

  override def numberOfRegisters: Int = specification.numberOfRegisters

  override def startAddress: Int = specification.startAddress
}


