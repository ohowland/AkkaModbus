package CGC.AssetModels

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest._
import scala.concurrent.duration._

class ModbusCommSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ModbusCommSpec"))

  "A Modbus Communication Actor" should {
    "Respond to poll request " in {

      val testModbusRegister = ModbusComm.ModbusRegsiter(
        name = "Test",
        address = 1,
        datatype = ModbusComm.U32)

      val testConfig = ModbusComm.ModbusConfig(
        hostName = "localhost",
        port = 502,
        id = 1,
        timeout = 3,
        maxRetryAttempts = 2,
        registers = testModbusRegister :: List.empty)

      val commActor = system.actorOf(ModbusComm.props(testConfig))
      val probe = TestProbe()

      commActor.tell(ModbusComm.ReqPollSlave(requestId = 42), probe.ref)
      val response = probe.expectMsgType[ModbusComm.RespPollSlave]
      response.requestId should ===(42)
    }
  }
}
