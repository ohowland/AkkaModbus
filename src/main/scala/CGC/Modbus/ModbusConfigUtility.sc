import CGC.Modbus.MessageFactory._
import CGC.Modbus.ConfigReader._
import CGC.Modbus.ModbusTypes.ModbusDatatype

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

import CGC.Modbus.ModbusTypes
import spray.json._
import spray.json.DefaultJsonProtocol._


object ExtendedJsonProtocol {
  implicit def listJsonWriter[T: JsonWriter]: RootJsonWriter[List[T]] = new RootJsonWriter[List[T]] {
    def write(list: List[T]): JsArray = JsArray(list.map(_.toJson).toVector)
  }
}

import ExtendedJsonProtocol._
case class TestReg(name: String, value: Int)

implicit val testRegJsonFormat = jsonFormat2(TestReg)
val reg1 = TestReg("register1", 11)
val reg2 = TestReg("register2", 22)
List(reg1, reg2).toJson