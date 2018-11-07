package modbus.frame

import org.scalatest._

class DecodeSpec() extends Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  "An Application Data Unit Decoder" should {
    "interpret the ByteString representation of a ResponseReadHoldingRegisters" in {
      val transactionId = 321
      val unitId = 10
      val registerList = List(1, 2, 3, 4)
      val byteCount = registerList.length * 2

      val testPDU: PDU = ResponseReadHoldingRegisters(byteCount, registerList)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = Decode.adu(response)

      adu should ===(ADU(
        MBAP(transactionId, testPDU.length + 1, unitId),
        ResponseReadHoldingRegisters(byteCount, registerList)))
    }

    "interpret the ByteString representation of an ExceptionReadHoldingRegisters" in {
      val transactionId = 123
      val unitId = 33
      val errorCode = 3

      val testPDU: PDU = ExceptionReadHoldingRegisters(errorCode.toByte)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = Decode.adu(response)

      adu should ===(ADU(MBAP(transactionId, testPDU.length + 1, unitId), ExceptionReadHoldingRegisters(errorCode)))
    }

    "interpret the ByteString representation of a ResponseWriteHoldingRegisters" in {
      val transactionId = 100
      val unitId = 1
      val startAddress = 0
      val numberOfRegisters = 30

      val testPDU: PDU = ResponseWriteHoldingRegisters(startAddress, numberOfRegisters)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = Decode.adu(response)

      adu should ===(ADU(
        MBAP(transactionId, testPDU.length + 1, unitId),
        ResponseWriteHoldingRegisters(startAddress, numberOfRegisters)))
    }

    "interpret the ByteString respresentation of an ExceptionWriteHoldingRegisters" in {
      val transactionId = 999
      val unitId = 126
      val errorCode = 4

      val testPDU: PDU = ExceptionWriteHoldingRegisters(errorCode.toByte)
      val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length, unitId)
      val testADU: ADU = ADU(testMBAP, testPDU)

      val response = testADU.toByteString
      val adu: ADU = Decode.adu(response)

      adu should ===(ADU(MBAP(transactionId, 2, unitId), ExceptionWriteHoldingRegisters(errorCode)))
    }
  }
}

