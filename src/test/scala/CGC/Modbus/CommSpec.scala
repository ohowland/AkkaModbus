package CGC.Modbus

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest._

import scala.concurrent.duration._

class CommSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("CommSpec"))

  "A Modbus Communication Actor" should {
    "Respond to poll request " in {

      val testConfig = Comm.ModbusConfig(
        hostName = "localhost",
        port = 502,
        id = 1,
        timeout = 3)

      val commActor = system.actorOf(Comm.props(testConfig))
      val probe = TestProbe()

      commActor.tell(Comm.ReqReadHoldingRegisters(
        requestId = 42,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
      val response = probe.expectMsgType[Comm.RespReadHoldingRegisters]
      response.requestId should ===(42)
    }
  }
}
