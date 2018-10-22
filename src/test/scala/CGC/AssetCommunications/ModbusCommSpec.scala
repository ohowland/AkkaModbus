package CGC.AssetCommunications

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

      val testConfig = Comm.ModbusConfig(
        hostName = "localhost",
        port = 502,
        id = 1,
        timeout = 3,
        maxRetryAttempts = 2)

      val commActor = system.actorOf(Comm.props(testConfig))
      val probe = TestProbe()

      commActor.tell(Comm.ReqReadHoldingRegisters(
        requestId = 42,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
      val response = probe.expectMsgType[Comm.RespReadHoldingRegisters]
      response.requestId should ===(42)
    }

    "timeout if host is unavailable " in {

      val testConfig = Comm.ModbusConfig(
        hostName = "no.love",
        port = 502,
        id = 1,
        timeout = 1,
        maxRetryAttempts = 1)

      val commActor = system.actorOf(Comm.props(testConfig))
      val probe = TestProbe()

      commActor.tell(Comm.ReqReadHoldingRegisters(
        requestId = 42,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
        probe.expectMsg(30.seconds, Comm.ConnectionTimedOut(42))
    }

    "respond with a timeout message if in the timeout state " in {

      val testConfig = Comm.ModbusConfig(
        hostName = "no.love",
        port = 502,
        id = 1,
        timeout = 1,
        maxRetryAttempts = 1)

      val commActor = system.actorOf(Comm.props(testConfig))
      val probe = TestProbe()
      // this message will timeout the actor
      commActor.tell(Comm.ReqReadHoldingRegisters(
        requestId = 42,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
      probe.expectMsg(30.seconds, Comm.ConnectionTimedOut(42))

      // actor should respond immediately with a timeout message
      commActor.tell(Comm.ReqReadHoldingRegisters(
        requestId = 1,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
      val response = probe.expectMsgType[Comm.ConnectionTimedOut]
      response.requestId should === (1)

    }
  }
}
