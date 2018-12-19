package modbus.template

class WriteSingleHoldingRegisterTemplate(specification: Template.BlockSpecification,
                                         register: Modbus.Register,
                                         endianness: String) extends Template {
  override def encode(request: Map[String, Double]): List[Int] = ???

  override def numberOfRegisters: Int = specification.numberOfRegisters

  override def startAddress: Int = specification.startAddress
}

