package CGC.AssetCommunications

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest._

import scala.concurrent.duration._

class ModbusReadActorSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ModbusCommSpec"))

  "A ModbusReadQuery Actor" should {
    "return a status value map for working devices" in {
      val requester = TestProbe()
      val modbusActor = TestProbe()

      val testRegister1 = ModbusCommActor.ModbusRegsiter(
        "test1",
        1,
        ModbusCommActor.U16,
        ModbusCommActor.Read,
        "status",
        1)

      val testModbusMap = List(testRegister1)
      val reqMessageTemplates = ModbusMessageFactory.createModbusMessageTemplates(testModbusMap, "status")

      val queryActor = system.actorOf(ModbusReadActor.props(
        requestId = 1,
        targetActor = modbusActor.ref,
        messageList = reqMessageTemplates,
        modbusMap = testModbusMap,
        requester = requester.ref,
        timeout = 3.seconds
      ))

      modbusActor.expectMsgType[ModbusCommActor.ReqReadHoldingRegisters]
      queryActor.tell(ModbusCommActor.RespReadHoldingRegisters(1, Some(List(11, 22))), modbusActor.ref)

      requester.expectMsg(AssetActor.StatusMessage(
        requestId = 1,
        status = Map(
          "test1" -> 11,
          "test2" -> 22)))
    }
  }
}