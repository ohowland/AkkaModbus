package modbus.poll

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import modbus.poll.Poll.PollResponse
import org.scalatest._

import scala.concurrent.duration._

class PollSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ReadSpec"))

  "A Modbus Poll Actor" should {
    "return a status value map for working devices" in {
      val requester = TestProbe()
      val clientActor = TestProbe()
      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)

      val testModbusMap = List(testRegister1)
      val reqMessageTemplates =
        MessageFactory.createReadModbusMessageTemplates(testModbusMap, "status", "big")

      val pollActor = system.actorOf(Poll.props(
        requestId = 1,
        clientHandler = clientActor.ref,
        messages = reqMessageTemplates.toSet,
        unitId = 1,
        requester = requester.ref,
        timeout = 3.seconds
      ))

      requester.expectMsgType[PollResponse]
    }
  }
}
