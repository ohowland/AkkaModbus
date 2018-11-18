package modbus.frame

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest._
import akka.util.ByteString

  class FrameSpec(_system: ActorSystem) extends TestKit(_system)
    with Matchers
    with WordSpecLike
    with BeforeAndAfterAll {

    def this() = this(ActorSystem("ADUFramerSpec"))

    "An Application Data Unit Framer" should {
      "properly format the MBAP header" in {
        val transactionId = 123
        val unitId = 1

        val testPDU: PDU = PDU.empty
        val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
        val testADU: ADU = ADU(testMBAP, testPDU)

        val response = testADU.toByteString
        val MBAPFrameResponse = response take 7 // the MBAP header is the first 7 bytes of the ADU frame
        MBAPFrameResponse should ===(ByteString(0, transactionId, 0, 0, 0, testPDU.length + 1, unitId))
      }

      "convert a RequestReadHoldingRegisters into properly formatted ByteString" in {
        val transactionId = 321
        val unitId = 10
        val numberOfRegisters = 2
        val startAddress = 1

        val testPDU: PDU = RequestReadHoldingRegisters(startAddress, numberOfRegisters)
        val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
        val testADU: ADU = ADU(testMBAP, testPDU)

        val response = testADU.toByteString

        val MBAPFrameResponse = response take 7 // the MBAP header is the first 7 bytes of the ADU frame
        MBAPFrameResponse should ===(
          ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId))

        val PDUFrameResponse = response drop 7 // the PDU is the everything after the first 7 bytes of ADU
        PDUFrameResponse should ===(ByteString(0x3, 0, startAddress, 0, numberOfRegisters))
      }

      "convert a ResponseReadHoldingRegisters into properly formatted ByteString" in {
        fail("not implemented")
      }

      "convert an ExceptionReadHoldingRegisters into a properly formatted ByteString" in {
        val transactionId = 321
        val unitId = 10
        val errorCode = 3

        val testPDU: PDU = ExceptionReadHoldingRegisters(errorCode)
        val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
        val testADU: ADU = ADU(testMBAP, testPDU)

        val response = testADU.toByteString

        val MBAPFrameResponse = response take 7 // the MBAP header is the first 7 bytes of the ADU frame
        MBAPFrameResponse should ===(
          ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId))

        val PDUFrameResponse = response drop 7 // the PDU is the everything after the first 7 bytes of ADU
        PDUFrameResponse should ===(ByteString(0x83, errorCode))
      }

      "convert a RequestWriteHoldingRegisters into properly formatted ByteStreams" in {
        val transactionId = 999
        val unitId = 10
        val payload = List(1, 2, 3, 4, 5)
        val numberOfRegisters = payload.length
        val startAddress = 22

        val testPDU: PDU =
          RequestWriteHoldingRegisters(startAddress, numberOfRegisters, payload)
        val testMBAP: MBAP = MBAP(transactionId, testPDU.length + 1, unitId)
        val testADU: ADU = ADU(testMBAP, testPDU)

        val response = testADU.toByteString

        val MBAPFrameResponse = response take 7 // the MBAP header is the first 7 bytes of the ADU frame
        MBAPFrameResponse should ===(
          ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId))
        val PDUFrameResponse = response drop 7 // the PDU is the everything after the first 7 bytes of ADU
        PDUFrameResponse should ===(
          ByteString(0x10, 0, startAddress, 0, numberOfRegisters, numberOfRegisters * 2, 0, payload(0), 0, payload(1),
          0, payload(2), 0, payload(3), 0, payload(4)))
      }

      "convert a ResponseWriteHoldingRegisters into properly formatted ByteString" in {
        fail("not implemented")
      }

      "convert a ExceptionWriteHoldingRegisters into properly formatted ByteString" in {
        fail("not implemented")
      }

    }
  }

