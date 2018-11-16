package modbus.poll

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import modbus.frame._
import modbus.poll.Poll.PollResponse
import org.scalatest._

import scala.concurrent.duration._

class PollSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ReadSpec"))

  "A Modbus Poll Actor" should {
    "return a decoded bytestring" in {
      val requester = TestProbe()
      val clientHandler = TestProbe()
      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)

      val testModbusMap = List(testRegister1)
      val reqMessageTemplates =
        MessageFactory.createReadModbusMessageTemplates(testModbusMap, "status", "big")

      val pollActor = system.actorOf(Poll.props(
        requestId = 33,
        clientHandler = clientHandler.ref,
        messages = reqMessageTemplates.toSet,
        unitId = 1,
        requester = requester.ref,
        timeout = 3.seconds
      ))

      // The poll actor sends the clientHandler actor an ADU constructed from the message template
      val requestADU = clientHandler.expectMsgType[ADU]

      // In the successful case, the clientHandler will return a
      // ByteString representation of a ResponseReadHoldingRegisters
      val responsePDU = ResponseReadHoldingRegisters(reqMessageTemplates.size * 2, List(11))
      val responseByteString =
        ADU(MBAP(requestADU.mbap.transactionId, 0, requestADU.mbap.unitId), responsePDU).toByteString
      pollActor ! responseByteString

      // Poll handles decoding the ResponseReadHoldingRegisters ByteString back to a Map(String -> Double)
      // Defined by the ModbusTemplate
      val responseMap = requester.expectMsgType[PollResponse]
      responseMap.requestId should ===(33)
      responseMap.namedValueMap should===( Map("test1" -> 11.0) )
    }
  }
}
