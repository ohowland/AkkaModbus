package modbus.template

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest._

class MessageFactorySpec extends Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  "A CSV ConfigReader" should {
    "load from a ModbusMap from class resources by name" in {
      val ModbusMap: List[Modbus.Register] = ConfigReader.readResource("testModbusMap.csv")
      ModbusMap should ===(List(
        Modbus.Register("test1", 10, Modbus.U16, "status", 0, 1),
        Modbus.Register("test2", 11, Modbus.U32, "status", 0, 1),
        Modbus.Register("test3", 15, Modbus.I16, "status", 1, 1),
        Modbus.Register("test4", 16, Modbus.F32, "status", 1, 1),
        Modbus.Register("test5", 20, Modbus.F64, "status", 2, 1)))
    }
  }

  "A Modbus Template Factory" should {
    "return a single template" in {
      fail("not implemented")
    }
  }
}
