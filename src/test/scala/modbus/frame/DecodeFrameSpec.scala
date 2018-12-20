package modbus.frame

import org.scalatest._

import scala.util.Random
import modbus.frame._

class DecodeFrameSpec() extends Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  "An Application Data Unit Decoder" should {
    "interpret the ByteString representation of a ResponseReadHoldingRegisters" in {
      val transactionId = Random.nextInt(65535) & 0xFF
      val unitId = 10
      val registerList = List(1, 2, 3, 4)
      val byteCount = registerList.length * 2

      val testPDU: PDU = ResponseReadMultipleHoldingRegisters(byteCount, registerList)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = ADU.decode(response)

      println(s"working bytestring: ${adu.pdu.payload}")
      adu should ===(ADU(
        MBAP(transactionId, testPDU.length + 1, unitId),
        ResponseReadMultipleHoldingRegisters(byteCount, registerList)))
    }

    "interpret the ByteString representation of an ExceptionReadHoldingRegisters" in {
      val transactionId = 123
      val unitId = 33
      val errorCode = 3

      val testPDU: PDU = ExceptionReadMultipleHoldingRegisters(errorCode.toByte)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = ADU.decode(response)

      adu should ===(ADU(MBAP(transactionId, testPDU.length + 1, unitId), ExceptionReadMultipleHoldingRegisters(errorCode)))
    }

    //TODO: Is this the behavior we want?
    "return an empty payload if the ByteString representation of A ResponseReadHoldingRegisters has " +
      "incorrect byte length" in {
      val transactionId = Random.nextInt(65535) & 0xFF
      val unitId = 1
      val registerList = List(1, 2, 3, 4)
      val byteCount = 0

      val testPDU: PDU = ResponseReadMultipleHoldingRegisters(byteCount, registerList)
      val testMBAP: MBAP = MBAP(transactionId, testPDU.length + 1, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = ADU.decode(response)

      adu should ===(ADU(
        MBAP(transactionId, testPDU.length + 1, unitId),
        ResponseReadMultipleHoldingRegisters(byteCount, List())))
    }

    "interpret the ByteString representation of a ResponseWriteMultipleHoldingRegisters" in {
      val transactionId = 100
      val unitId = 1
      val startAddress = 0
      val numberOfRegisters = 30

      val testPDU: PDU = ResponseWriteMultipleHoldingRegisters(startAddress, numberOfRegisters)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = ADU.decode(response)

      adu should ===(ADU(
        MBAP(transactionId, testPDU.length + 1, unitId),
        ResponseWriteMultipleHoldingRegisters(startAddress, numberOfRegisters)))
    }

    "interpret the ByteString representation of an ExceptionWriteHoldingRegisters" in {
      val transactionId = 999
      val unitId = 126
      val errorCode = 4

      val testPDU: PDU = ExceptionWriteMultipleHoldingRegisters(errorCode.toByte)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = ADU.decode(response)

      adu should ===(ADU(MBAP(transactionId, 2, unitId), ExceptionWriteMultipleHoldingRegisters(errorCode)))
    }
  }
}

