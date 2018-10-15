import com.typesafe.config._

val config: Config = ConfigFactory.load()

val name1 = config.getString("test.name")
println(s"first register: $name1")

import CGC.AssetCommunications.ModbusCSVReader
val fileURL = getClass.getResource("/testModbusMap.csv")
val modbusMap = ModbusCSVReader.readCSVToRegisterList(fileURL)

import CGC.AssetCommunications.ModbusMessageFactory
val templateMessages = ModbusMessageFactory.createReadHoldingRegisterMessageTemplates(modbusMap, "status")