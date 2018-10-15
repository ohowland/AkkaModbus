package CGC.AssetCommunications

object ModbusMessageFactory {

  case class ReadHoldingRegistersTemplate(startAddress: Int, numberOfRegisters: Int )

  /** Once a template message has been created, it can be used to generate modbus request messages
    * @param requestId
    * @param messageTemplate
    * @return
    */
  def createReqReadHoldingRegisterMessage(requestId: Long, messageTemplate: ReadHoldingRegistersTemplate):
  ModbusCommActor.ReqReadHoldingRegisters = {
    ModbusCommActor.ReqReadHoldingRegisters(
      requestId,
      messageTemplate.startAddress,
      messageTemplate.numberOfRegisters)
  }

  /** Devices use their own modbus map to create a set of template messages. These template messages can then
    * create ModbusComm.ModbusMessage types for querying an commanding the modbus device.
    * @param modbusMap List of all available modbus registers
    * @param groupName the list of returned ReadHoldRegister messages will only contain registers that belong to
    *                  this group.
    * @return
    */
  def createReadHoldingRegisterMessageTemplates(modbusMap: List[ModbusCommActor.ModbusRegsiter],
                                                groupName: String): Set[ReadHoldingRegistersTemplate] = {
    /** Blocks are a consecutive set of registers with size no larger than the maximum size defined in the
      * Modbus specification. The size is not enforced here, we assume the blocks in the specifcation file have
      * been sized correctly.
      */
    val blocks = modbusMap.filter(_.group == groupName).map(_.block).toSet

    val blockAddressLists = for {
        block <- blocks
        addressList = modbusMap.filter(_.block == block).map(_.address)
    } yield addressList

    for {
      addressList: List[Int] <- blockAddressLists
      startAddress: Int = addressList.min
      lastAddress: Int = addressList.max
      lastAddressType: ModbusCommActor.ModbusDatatype = modbusMap.filter(_.address == lastAddress).map(_.datatype).head
      numberOfRegisters: Int = lastAddress + modbusDatatypeWords(lastAddressType) - startAddress
    } yield ReadHoldingRegistersTemplate(startAddress, numberOfRegisters)
  }

  /**
    *
    * @param datatype
    * @return
    */
  def modbusDatatypeWords(datatype: ModbusCommActor.ModbusDatatype): Int = datatype match {
    case ModbusCommActor.U16 => 1
    case ModbusCommActor.U32 => 2
    case ModbusCommActor.I16 => 1
    case ModbusCommActor.I32 => 2
    case ModbusCommActor.F32 => 2
    case ModbusCommActor.F64 => 4
  }
}
