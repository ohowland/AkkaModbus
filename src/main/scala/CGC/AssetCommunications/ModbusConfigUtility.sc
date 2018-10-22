import CGC.AssetCommunications.MessageFactory._


// Load a Modbus Map
import CGC.AssetCommunications.ModbusMapReader
val fileURL = getClass.getResource("/testModbusMap.csv")
val modbusMap = ModbusMapReader.readCSVToRegisterList(fileURL)

// Create templates for the ReqReadHoldingRegister mesage
import CGC.AssetCommunications.MessageFactory
val msgs = createModbusMessageTemplates(modbusMap, "status")

// Generate fake ReqReadHoldingRegister response data, List[Short]
val response: List[Int] = List(12, 0, 1)

for {
  msg <- msgs
  value = msg.decode(response)
} yield value
