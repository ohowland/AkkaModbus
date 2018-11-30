package modbus.templates

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest._

class MessageFactorySpec extends Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  "A CSV ConfigReader" should {
    "load from a ModbusMap from class resources by name" in {
      val ModbusMap: List[ModbusTypes.ModbusRegister] = ConfigReader.readResource("testModbusMap.csv")
      ModbusMap should ===(List(
        ModbusTypes.ModbusRegister("test1", 10, ModbusTypes.U16, "status", 0),
        ModbusTypes.ModbusRegister("test2", 11, ModbusTypes.U32, "status", 0),
        ModbusTypes.ModbusRegister("test3", 15, ModbusTypes.I16, "status", 1),
        ModbusTypes.ModbusRegister("test4", 16, ModbusTypes.F32, "status", 1),
        ModbusTypes.ModbusRegister("test5", 20, ModbusTypes.F64, "status", 2)))
    }
  }

  "A Modbus Template Factory" should {
    "return a single template" in {
      fail("not implemented")
    }
  }
}
