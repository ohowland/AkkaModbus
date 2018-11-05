import java.net.InetSocketAddress

import Modbus.Frame.{ADU, MBAP, PDU, RequestReadHoldingRegisters}

val transactionId = 1
val unitId = 1
val numberOfRegisters = 1
val startAddress = 0

val testPDU: PDU =
  RequestReadHoldingRegisters(startAddress, numberOfRegisters)

testPDU.toByteString

val testMBAP: MBAP = MBAP(transactionId, length = testPDU.length + 1, unitId)
val testADU: ADU = ADU(testMBAP, testPDU)

testADU.toByteString