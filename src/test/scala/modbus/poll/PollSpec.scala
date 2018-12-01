package modbus.poll

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import modbus.frame._
import modbus.io.Client
import modbus.poll.Poll.PollResponse
import modbus.templates.{Factory, ModbusTypes}
import org.scalatest._

import scala.concurrent.duration._

class PollSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ReadSpec"))

  "A Modbus Poll Actor" should {
    "return a decoded valueMap from a single ReadHoldingRegistersRequest containing a single register" in {
      val requester = TestProbe()
      val clientHandler = TestProbe()
      val requestId = util.Random.nextInt

      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)
      val testRegister2 = ModbusTypes.ModbusRegister("badtest1", 2, ModbusTypes.U16, "control", 1)
      val testRegister3 = ModbusTypes.ModbusRegister("badtest2", 3, ModbusTypes.U16, "config", 1)

      val testModbusMap = List(testRegister1, testRegister2, testRegister3)
      val reqMessageTemplates =
        Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, "status", "big")

      val pollActor = system.actorOf(Poll.props(
        requestId = requestId,
        clientHandler = clientHandler.ref,
        templates = reqMessageTemplates.toSet,
        unitId = 1,
        requester = requester.ref,
        timeout = 3.seconds
      ))

      // The poll actor sends the clientHandler actor a ByteString constructed from the message template
      val requestByteString = clientHandler.expectMsgType[Client.Write]

      // Convert to ADU so we can access the transactionID, which is assigned by the client handler.
      val requestADU = DecodeFrame.decode(requestByteString.data)

      // In the successful case, the clientHandler will return a
      // ByteString representation of a ResponseReadHoldingRegisters
      val responsePDU = ResponseReadHoldingRegisters(reqMessageTemplates.head.numberOfRegisters * 2, List(11))
      val responseByteString =
        ADU(MBAP(requestADU.mbap.transactionId, responsePDU.length + 1, requestADU.mbap.unitId), responsePDU).toByteString
      pollActor ! responseByteString

      // Poll handles decoding the ResponseReadHoldingRegisters ByteString back to a Map(String -> Double)
      // Defined by the ModbusTemplate
      val responseMap = requester.expectMsgType[PollResponse]
      responseMap.requestId should ===(requestId)
      responseMap.namedValueMap should===( Map("test1" -> 11.0) )
    }

    "return a decoded valueMap from a single ReadHoldingRegistersRequest containing " +
      "multiple sequential registers" in {
      val requester = TestProbe()
      val clientHandler = TestProbe()
      val requestId = util.Random.nextInt

      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)
      val testRegister2 = ModbusTypes.ModbusRegister("test2", 2, ModbusTypes.U16, "status", 1)
      val testRegister3 = ModbusTypes.ModbusRegister("badtest1", 3, ModbusTypes.U16, "control", 2)
      val testModbusMap = List(testRegister1, testRegister2, testRegister3)
      val reqMessageTemplates =
        Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, "status", "big")

      val pollActor = system.actorOf(Poll.props(
        requestId = requestId,
        clientHandler = clientHandler.ref,
        templates = reqMessageTemplates.toSet,
        unitId = 1,
        requester = requester.ref,
        timeout = 3.seconds
      ))

      // The poll actor sends the clientHandler actor an ADU constructed from the message template
      val requestByteString = clientHandler.expectMsgType[Client.Write]
      val requestADU = DecodeFrame.decode(requestByteString.data)

      // In the successful case, the clientHandler will return a
      // ByteString representation of a ResponseReadHoldingRegisters
      val responsePDU = ResponseReadHoldingRegisters(reqMessageTemplates.head.numberOfRegisters * 2, List(1, 2))
      val responseByteString =
        ADU(MBAP(requestADU.mbap.transactionId, 0, requestADU.mbap.unitId), responsePDU).toByteString
      pollActor ! responseByteString

      // Poll handles decoding the ResponseReadHoldingRegisters ByteString back to a Map(String -> Double)
      // Defined by the ModbusTemplate
      val responseMap = requester.expectMsgType[PollResponse]
      responseMap.requestId should ===(requestId)
      responseMap.namedValueMap should===( Map("test1" -> 1.0, "test2" -> 2.0) )
    }

    "return a decoded valueMap from a single ReadHoldingRegistersRequest containing " +
      "multiple non-sequential registers" in {
      val requester = TestProbe()
      val clientHandler = TestProbe()
      val requestId = util.Random.nextInt

      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)
      val testRegister2 = ModbusTypes.ModbusRegister("test2", 5, ModbusTypes.U16, "status", 1)
      val testRegister3 = ModbusTypes.ModbusRegister("test3", 10, ModbusTypes.U16, "status", 1)
      val testModbusMap = List(testRegister1, testRegister2, testRegister3)
      val reqMessageTemplates =
        Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, "status", "big")

      val pollActor = system.actorOf(Poll.props(
        requestId = requestId,
        clientHandler = clientHandler.ref,
        templates = reqMessageTemplates.toSet,
        unitId = 1,
        requester = requester.ref,
        timeout = 3.seconds
      ))

      // The poll actor sends the clientHandler actor an ADU constructed from the message template
      val requestByteString = clientHandler.expectMsgType[Client.Write]
      val requestADU = DecodeFrame.decode(requestByteString.data)


      // In the successful case, the clientHandler will return a
      // ByteString representation of a ResponseReadHoldingRegisters
      val responsePDU = ResponseReadHoldingRegisters(
        reqMessageTemplates.head.numberOfRegisters * 2, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      val responseByteString =
        ADU(MBAP(requestADU.mbap.transactionId, 0, requestADU.mbap.unitId), responsePDU).toByteString
      pollActor ! responseByteString

      // Poll handles decoding the ResponseReadHoldingRegisters ByteString back to a Map(String -> Double)
      // Defined by the ModbusTemplate
      val responseMap = requester.expectMsgType[PollResponse]
      responseMap.requestId should ===(requestId)
      responseMap.namedValueMap should===( Map("test1" -> 1.0, "test2" -> 5.0, "test3" -> 10.0) )
    }

    "return a decoded valueMap from multiple ReadHoldingRegistersRequest containing " +
      "a single register each" in {
      val requester = TestProbe()
      val clientHandler = TestProbe()
      val requestId = util.Random.nextInt

      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)
      val testRegister2 = ModbusTypes.ModbusRegister("test2", 2, ModbusTypes.U16, "status", 2)
      val testRegister3 = ModbusTypes.ModbusRegister("badtest1", 3, ModbusTypes.U16, "control", 3)

      val testModbusMap = List(testRegister1, testRegister2)
      val reqMessageTemplates =
        Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, "status", "big")

      val pollActor = system.actorOf(Poll.props(
        requestId = requestId,
        clientHandler = clientHandler.ref,
        templates = reqMessageTemplates.toSet,
        unitId = 1,
        requester = requester.ref,
        timeout = 3.seconds
      ))

      // The poll actor sends the clientHandler actor an ADU constructed from the message template.
      val requestByteString1 = clientHandler.expectMsgType[Client.Write]
      val requestADU1 = DecodeFrame.decode(requestByteString1.data)
      val requestByteString2 = clientHandler.expectMsgType[Client.Write]
      val requestADU2 = DecodeFrame.decode(requestByteString2.data)

      // In the successful case, the clientHandler will return a
      // ByteString representation of a ResponseReadHoldingRegisters
      val responsePDU1 = ResponseReadHoldingRegisters(
        reqMessageTemplates(0).numberOfRegisters * 2, List(1))
      val responseByteString1 =
        ADU(MBAP(requestADU1.mbap.transactionId, 0, requestADU1.mbap.unitId), responsePDU1).toByteString
      pollActor ! responseByteString1

      val responsePDU2 = ResponseReadHoldingRegisters(
        reqMessageTemplates(1).numberOfRegisters * 2, List(2))
      val responseByteString2 =
        ADU(MBAP(requestADU2.mbap.transactionId, 0, requestADU2.mbap.unitId), responsePDU2).toByteString
      pollActor ! responseByteString2

      // Poll handles decoding the ResponseReadHoldingRegisters ByteString back to a Map(String -> Double)
      // Defined by the ModbusTemplate
      val responseMap = requester.expectMsgType[PollResponse]
      responseMap.requestId should ===(requestId)
      responseMap.namedValueMap should===( Map("test1" -> 1.0, "test2" -> 2.0) )
    }

    "handle a ExceptionReadHoldingRegisters in a single request" in {
      fail("not implemented")
    }

    "handle a ExceptionReadHoldingRegisters in a multi request" in {
      fail("not implemented")
    }

    "handle a clientHandler timeout" in {
      fail("not implemented")
    }

    "handle a clientHandler terminated" in {
      fail("not implemented")
    }
  }
}
