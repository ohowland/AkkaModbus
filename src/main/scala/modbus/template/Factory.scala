package modbus.template

object Builder {

  def setModbusMap(m: List[Modbus.Register]) = ???
  def setGroupName(gn: String)
  def setEndianness()
  def execute()

  def getReadMultipleHoldingRegistersTemplates(modbusMap: List[Modbus.Register],
                                               groupName: String,
                                               endianness: String): List[Template] = {

    def findBlocks(modbusMap: List[Modbus.Register], groupName: String): Set[Int] =
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
    def bucketRegisterListByBlock(modbusMap: List[Modbus.Register],
                                  blocks: Set[Int],
                                  groupName: String): List[List[Modbus.Register]] = {
      (for {
        block <- blocks
        registerList = modbusMap.filter(_.group == groupName).filter(_.block == block)
      } yield registerList).toList
    }

    def buildBlockSpecification(registerList: List[Modbus.Register]): Template.BlockSpecification = {
      val startAddress: Int = registerList.map(_.address).min
      val lastAddress: Int = registerList.map(_.address).max
      val lastAddressType: Modbus.Datatype = modbusMap.filter(_.address == lastAddress).map(_.datatype).head
      val numberOfRegisters: Int = lastAddress + lastAddressType.nWords - startAddress
      Template.BlockSpecification(startAddress, numberOfRegisters)
    }

    for {
      registerBlock <- bucketRegisterListByBlock(modbusMap, findBlocks(modbusMap, groupName), groupName)
    } yield ReadMultipleHoldingRegistersTemplate(buildBlockSpecification(registerBlock), registerBlock, endianness)
  }

  def getWriteMultipleHoldingRegistersTemplates(modbusMap: List[Modbus.Register],
                                                groupName: String,
                                                endianness: String): List[Template] = {

    def findBlocks(modbusMap: List[Modbus.Register], groupName: String): Set[Int] =
      modbusMap.filter(_.group == groupName).map(_.block).toSet

    def bucketRegisterListByBlock(modbusMap: List[Modbus.Register],
                                  blocks: Set[Int],
                                  groupName: String): List[List[Modbus.Register]] = {
      (for {
        block <- blocks
        registerList = modbusMap.filter(_.group == groupName).filter(_.block == block)
      } yield registerList).toList
    }

    def buildBlockSpecification(registerList: List[Modbus.Register]): Template.BlockSpecification = {
      val startAddress: Int = registerList.map(_.address).min
      val lastAddress: Int = registerList.map(_.address).max
      val lastAddressType: Modbus.Datatype = modbusMap.filter(_.address == lastAddress).map(_.datatype).head
      val numberOfRegisters: Int = lastAddress + lastAddressType.nWords - startAddress
      Template.BlockSpecification(startAddress, numberOfRegisters)
    }

    for {
      registerBlock <- bucketRegisterListByBlock(modbusMap, findBlocks(modbusMap, groupName), groupName)
    } yield WriteMultipleHoldingRegistersTemplate(buildBlockSpecification(registerBlock), registerBlock, endianness)
  }

  def getWriteSingleHoldingRegisterTemplates(modbusMap: List[Modbus.Register],
                                             groupName: String,
                                             endianness: String): List[Template] = {

    def buildBlockSpecification(register: Modbus.Register) = {
      Template.BlockSpecification(register.address, 1)
    }

    for {
      register <- modbusMap.filter(_.group == groupName)
    } yield WriteSingleHoldingRegisterTemplate(buildBlockSpecification(register), register, endianness)
  }
}