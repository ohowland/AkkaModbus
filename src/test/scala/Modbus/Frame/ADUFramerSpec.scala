package Modbus.Frame

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest._
import akka.util.ByteString

  class ADUFramerSpec(_system: ActorSystem) extends TestKit(_system)
    with Matchers
    with WordSpecLike
    with BeforeAndAfterAll {

    def this() = this(ActorSystem("ADUFramerSpec"))

    "An Application Data Unit Framer" should {
      "properly format the MBAP header" in {
        val aduFramer = system.actorOf(ADUFramer.props)
        val probe = TestProbe()

        val transactionId = 123
        val unitId = 1

        val testPDU: PDU = PDU.empty
        val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length, unitId)
        val testADU: ADU = ADU(testMBAP, testPDU)

        aduFramer.tell(testADU, probe.ref)
        val response = probe.expectMsgType[ADUFramer.ADUByteString]
        val MBAPFrameResponse = response.aduByteString take 7 // the MBAP header is the first 7 bytes of the ADU frame
        MBAPFrameResponse should ===(ByteString(0, transactionId, 0, 0, 0, 0, unitId))
      }

      "convert a RequestReadHoldingRegisters objects into properly formatted ByteStreams" in {
        val aduFramer = system.actorOf(ADUFramer.props)
        val probe = TestProbe()

        val transactionId = 321
        val unitId = 10
        val numberOfRegisters = 2
        val startAddress = 1

        val testPDU: PDU = RequestReadHoldingRegisters(startAddress, numberOfRegisters)
        val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length, unitId)
        val testADU: ADU = ADU(testMBAP, testPDU)

        aduFramer.tell(testADU, probe.ref)
        val response = probe.expectMsgType[ADUFramer.ADUByteString]

        val MBAPFrameResponse = response.aduByteString take 7 // the MBAP header is the first 7 bytes of the ADU frame
        MBAPFrameResponse should ===(
          ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId))

        val PDUFrameResponse = response.aduByteString drop 7 // the PDU is the everything after the first 7 bytes of ADU
        PDUFrameResponse should ===(ByteString(0x3, 0, startAddress, 0, numberOfRegisters))
      }

      "convert a WriteReadHoldingRegisters objects into properly formatted ByteStreams" in {
        val aduFramer = system.actorOf(ADUFramer.props)
        val probe = TestProbe()

        val transactionId = 999
        val unitId = 10
        val payload = List(1, 2, 3, 4, 5)
        val numberOfRegisters = payload.length
        val startAddress = 22

        val testPDU: PDU =
          RequestWriteHoldingRegisters(startAddress, numberOfRegisters, payload)
        val testMBAP: MBAP = MBAP(transactionId, testPDU.length + 1, unitId)
        val testADU: ADU = ADU(testMBAP, testPDU)

        aduFramer.tell(testADU, probe.ref)
        val response = probe.expectMsgType[ADUFramer.ADUByteString]

        val MBAPFrameResponse = response.aduByteString take 7 // the MBAP header is the first 7 bytes of the ADU frame
        MBAPFrameResponse should ===(
          ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId))
        val PDUFrameResponse = response.aduByteString drop 7 // the PDU is the everything after the first 7 bytes of ADU
        PDUFrameResponse should ===(
          ByteString(0x10, 0, startAddress, 0, numberOfRegisters, numberOfRegisters * 2, 0, payload(0), 0, payload(1),
          0, payload(2), 0, payload(3), 0, payload(4)))
      }
    }
  }

