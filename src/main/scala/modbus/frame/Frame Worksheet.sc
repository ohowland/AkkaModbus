import modbus.frame._

val transactionId = 999
val unitId = 10
val payload = List(1, 2, 3, 4, 5)
val numberOfRegisters = payload.length
val startAddress = 22

val testPDU: PDU =
  ExceptionWriteHoldingRegisters(3)
val testMBAP: MBAP = MBAP(transactionId, testPDU.length + 1, unitId)
val testADU: ADU = ADU(testMBAP, testPDU)

val response = testADU.toByteString

ExceptionWriteHoldingRegisters.functionCode.toByte.toInt & 0xFF


Decode.adu(response)