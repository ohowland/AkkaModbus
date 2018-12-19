package modbus.template

import scala.math.pow

object Factory {
  /**
    * Returns a list of ModbusTemplates configured with required data to complete a read holding registers
    * request.
    *
    * @param modbusMap is a list of ModbusRegisters which define communcation with a device.
    * @param groupName is used to select registers from the modbusMap based on the field ModbusRegister.group
    */
  def getReadMultipleHoldingRegistersTemplates(modbusMap: List[Modbus.Register],
                                               groupName: String,
                                               endianness: String): List[ModbusTemplate] = {
    /**
      * Returns a set of all blocks in the Modbus map for a given groupName.
      *
      * @param modbusMap is a list of ModbusRegisters which define communcation with a device.
      * @param groupName is used to select registers from the modbusMap based on the field ModbusRegister.group.
      */
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

    // Returns a ReadSpecification for the registerlist
    def buildBlockSpecification(registerList: List[Modbus.Register]): BlockSpecification = {
      val startAddress: Int = registerList.map(_.address).min
      val lastAddress: Int = registerList.map(_.address).max
      val lastAddressType: Modbus.Datatype = modbusMap.filter(_.address == lastAddress).map(_.datatype).head
      val numberOfRegisters: Int = lastAddress + lastAddressType.nWords - startAddress
      BlockSpecification(startAddress, numberOfRegisters)
    }

    for {
      registerBlock <- bucketRegisterListByBlock(modbusMap, findBlocks(modbusMap, groupName), groupName)
    } yield ReadMultipleHoldingRegistersTemplate(buildBlockSpecification(registerBlock), registerBlock, endianness)
  }

  // Returns a list of ModbusTemplates configured with required data to complete a write holding registers request.
  def getWriteMultipleHoldingRegistersTemplates(modbusMap: List[Modbus.Register],
                                                groupName: String,
                                                endianness: String): List[ModbusTemplate] = {

    // Returns a set of all blocks in the Modbus map for a given groupName.
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

    /**
      * Returns a BlockSpecification for the registerlist
      *
      * @param registerList
      * @return
      */
    def buildBlockSpecification(registerList: List[Modbus.Register]): BlockSpecification = {
      val startAddress: Int = registerList.map(_.address).min
      val lastAddress: Int = registerList.map(_.address).max
      val lastAddressType: Modbus.Datatype = modbusMap.filter(_.address == lastAddress).map(_.datatype).head
      val numberOfRegisters: Int = lastAddress + lastAddressType.nWords - startAddress
      BlockSpecification(startAddress, numberOfRegisters)
    }

    for {
      registerBlock <- bucketRegisterListByBlock(modbusMap, findBlocks(modbusMap, groupName), groupName)
    } yield WriteMultipleHoldingRegistersTemplate(buildBlockSpecification(registerBlock), registerBlock, endianness)
  }

  /**
    * Returns a list of ModbusTemplates configured configured with required data to complete a write single holding
    * register requests.
    *
    * @param modbusMap is a list of ModbusRegisters which define communcation with a device.
    * @param groupName is used to select registers from the modbusMap based on the field ModbusRegister.group
    */
  def getWriteSingleHoldingRegisterTemplates(modbusMap: List[Modbus.Register],
                                             groupName: String,
                                             endianness: String): List[ModbusTemplate] = {

    def buildBlockSpecification(register: Modbus.Register) = {
      BlockSpecification(register.address, 1)
    }

    for {
      register <- modbusMap.filter(_.group == groupName)
    } yield WriteSingleHoldingRegisterTemplate(buildBlockSpecification(register), register, endianness)
  }
}