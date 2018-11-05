import akka.util.{ByteIterator, ByteString, ByteStringBuilder}
import Modbus.Frame.{ADU, MBAP, RequestReadHoldingRegisters, RequestWriteHoldingRegisters}

val x: List[Int] = List(1, 2, 3)
val y: ByteString = x.foldLeft(ByteString.empty)(_ ++ ByteString(_))

val frameBuilder = ByteString.newBuilder
implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
frameBuilder.putByte(1.toByte)
frameBuilder.putInt(2)
frameBuilder.putByte(1.toByte)
frameBuilder.result()

val testReadPdu = RequestReadHoldingRegisters(startAddress = 23, numberOfRegisters =  1)
testReadPdu.toByteString
testReadPdu.length

val testWritePdu = RequestWriteHoldingRegisters(startAddress = 23, numberOfRegisters =  1, List(1))
testWritePdu.toByteString
testWritePdu.length

val testMbap = MBAP(transactionId = 12, length = 2, unitId = 1)
testMbap.toByteString


val testAdu = ADU(testMbap, testReadPdu)
testAdu.toByteString