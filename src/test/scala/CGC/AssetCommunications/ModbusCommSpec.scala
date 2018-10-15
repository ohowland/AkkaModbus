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

      val testConfig = ModbusComm.ModbusConfig(
        hostName = "localhost",
        port = 502,
        id = 1,
        timeout = 3,
        maxRetryAttempts = 2)

      val commActor = system.actorOf(ModbusComm.props(testConfig))
      val probe = TestProbe()

      commActor.tell(ModbusComm.ReqReadHoldingRegisters(
        requestId = 42,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
      val response = probe.expectMsgType[ModbusComm.RespReadHoldingRegisters]
      response.requestId should ===(42)
    }

    "timeout if host is unavailable " in {

      val testConfig = ModbusComm.ModbusConfig(
        hostName = "no.love",
        port = 502,
        id = 1,
        timeout = 1,
        maxRetryAttempts = 1)

      val commActor = system.actorOf(ModbusComm.props(testConfig))
      val probe = TestProbe()

      commActor.tell(ModbusComm.ReqReadHoldingRegisters(
        requestId = 42,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
        probe.expectMsg(30.seconds, ModbusComm.ConnectionTimedOut(42))
    }

    "respond with a timeout message if in the timeout state " in {

      val testConfig = ModbusComm.ModbusConfig(
        hostName = "no.love",
        port = 502,
        id = 1,
        timeout = 1,
        maxRetryAttempts = 1)

      val commActor = system.actorOf(ModbusComm.props(testConfig))
      val probe = TestProbe()
      // this message will timeout the actor
      commActor.tell(ModbusComm.ReqReadHoldingRegisters(
        requestId = 42,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
      probe.expectMsg(30.seconds, ModbusComm.ConnectionTimedOut(42))

      // actor should respond immediately with a timeout message
      commActor.tell(ModbusComm.ReqReadHoldingRegisters(
        requestId = 1,
        startAddress = 0,
        numberOfRegisters = 2), probe.ref)
      val response = probe.expectMsgType[ModbusComm.ConnectionTimedOut]
      response.requestId should === (1)

    }
  }
}
