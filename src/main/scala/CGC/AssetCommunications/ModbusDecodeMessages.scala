package CGC.AssetCommunications

class ModbusDecodeMessages {

  case class DecodeRespReadHoldingRegisterTemplate(registers: List[ModbusComm.ModbusRegsiter])  {
    def decode(response: List[Int]): Unit = {
      val startingAddress: Int = registers.head.address
      for {
        register <- registers
        index = register.address - startingAddress
        value = register.datatype match {
          case ModbusComm.U16 => List(response(index))
          case ModbusComm.I16 => List(response(index))
          case ModbusComm.U32 => List(response(index), response(index+1))
          case ModbusComm.I32 => List(response(index), response(index+1))
          case ModbusComm.F32 => List(response(index), response(index+1))
          case ModbusComm.F64 => List(response(index), response(index+1), response(index+2), response(index+3))
      }
      } yield Map(register.name -> value)
    }
  }

  def CreateRespHoldingRegisterMessageTemplate(requestTemplate: ModbusMessageFactory.ReadHoldingRegistersTemplate,
                                               modbusMap: List[ModbusComm.ModbusRegsiter]):
  DecodeRespReadHoldingRegisterTemplate = {
    // Get registers within the block read
    val sortedRegisterList = modbusMap.filter(_.address >= requestTemplate.startAddress )
      .filter(_.address <= (requestTemplate.startAddress + requestTemplate.numberOfRegisters))
      .sortBy(_.address)

    DecodeRespReadHoldingRegisterTemplate(sortedRegisterList)
}

}
