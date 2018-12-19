/**
  * MBAP header structure:
  * BYTES :       |     2 Bytes    |   2 Bytes   |      2 Bytes       | 1 Byte  |
  * DESCRIPTION : | Transaction Id | Protocol Id | PDU Length (bytes) | Unit Id |
  * @return
  */

package modbus.frame

import akka.util.ByteString

// MBAP Header for the Modbus ADU
case object MBAP {
  val protocolId: Int = 0

  def encode(transactionId: Int, length: Int, unitId: Int): MBAP = {
    MBAP(transactionId, length, unitId)
  }

  def decode(data: ByteString): MBAP = {
    val in = data.iterator
    val transactionId = in.getShort
    val protocolId = in.getShort
    val length = in.getShort
    val unitId = in.getByte
    MBAP(transactionId, length, unitId)
  }
}

case class MBAP(transactionId: Int, length: Int, unitId: Int) {
  def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putShort(transactionId)
    frameBuilder.putShort(MBAP.protocolId)
    frameBuilder.putShort(length)
    frameBuilder.putByte(unitId.toByte)
    frameBuilder.result()
  }
}

