import CGC.Modbus.MessageFactory._


// Load a Modbus Map
import CGC.Modbus.ConfigReader
val fileURL = getClass.getResource("/testModbusMap.csv")
val modbusMap = ConfigReader.readCSVToRegisterList(fileURL)

// Create templates for the ReqReadHoldingRegister mesage
import CGC.Modbus.MessageFactory
val msgs = createModbusMessageTemplates(modbusMap, "status")

// Generate fake ReqReadHoldingRegister response data, List[Short]
val response: List[Int] = List(12, 0, 1)

for {
  msg <- msgs
} yield msg.decode(response)
