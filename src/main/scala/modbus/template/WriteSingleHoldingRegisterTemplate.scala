package modbus.template

import scala.math.pow

case class WriteSingleHoldingRegisterTemplate(specification: Template.BlockSpecification,
                                              register: Modbus.Register,
                                              endianness: String) extends Template {

  override def encode(request: Map[String, Double]): List[Int] = {

    def decomposeDouble(value: Double, register: Modbus.Register): List[Int] = {
      def decomposeDoubleIter(value: BigInt, acc: List[Int]): List[Int] = {
        if (acc.size >= register.datatype.nWords) acc
        else {
          val mask = BigDecimal(0xFFFF * pow(2, 16 * acc.size)).toBigInt
          val nextWord = ((value & mask) >> (16 * acc.size)).toInt
          decomposeDoubleIter(value, nextWord :: acc)
        }
      }

      decomposeDoubleIter(BigDecimal(unscale(value, register)).toBigInt, List.empty)
    }

    List.empty
  }

  override def numberOfRegisters: Int = specification.numberOfRegisters

  override def startAddress: Int = specification.startAddress
}

