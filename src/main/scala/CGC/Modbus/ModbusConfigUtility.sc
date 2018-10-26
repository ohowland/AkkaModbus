import CGC.Modbus.MessageFactory._
import CGC.Modbus.ConfigReader._

// Load a Modbus Map
val fileURL = getClass.getResource("/testModbusMap.csv")
val modbusMap = readCSVToRegisterList(fileURL)
/**
  * test1,10,U16,status,0
  * test2,11,U32,status,0
  * test3,20,I16,status,1
  * test4,22,F32,status,1
  * test5,30,F64,status,2
  */

// Create templates for the ReqReadHoldingRegister mesage
val msgs = createModbusMessageTemplates(modbusMap, "status", "big")

// Generate fake ReqReadHoldingRegister response data, List[Short]
val response0: List[Int] = List(12, 0, 1)
val response1: List[Int] = List(5, 0, 2)
val response2: List[Int] = List(0, 0, 0, 5)

msgs(0).decode(response0)
msgs(1).decode(response1)
msgs(2).decode(response2)

