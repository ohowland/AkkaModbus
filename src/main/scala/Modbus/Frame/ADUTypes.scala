package Modbus.Frame

import akka.util.ByteString

/**
  * See the official Modbus specification here:
  * http://www.modbus.org/docs/Modbus_Messaging_Implementation_Guide_V1_0b.pdf
  */

/**
  * MBAP Header for the Modbus ADU
  *
  * @param transactionId : Identification of a MODBUS Request / Response transaction.
  * @param length        : Number of following bytes (i.e. the size of the PDU)
  * @param unitId        : Identification of a remote slave (aka Modbus ID)
  */
case class MBAP(transactionId: Int, length: Int, unitId: Int) {
  val protocolId: Int = 0 // always 0 for Modbus protocol

  /**
    * MBAP header structure:
    * BYTES :       |     2 Bytes    |   2 Bytes   |      2 Bytes       | 1 Byte  |
    * DESCRIPTION : | Transaction Id | Protocol Id | PDU Length (bytes) | Unit Id |
    * @return
    */
  def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putShort(transactionId)
    frameBuilder.putShort(protocolId)
    frameBuilder.putShort(length)
    frameBuilder.putByte(unitId.toByte)
    frameBuilder.result()
  }
}

/**
  * Modbus ADU (Application Data Unit)
  * BYTES:       |   7 Bytes   |          N Bytes         |
  * DESCRIPTION: | MBAP Header | Protocol Data Unit (PDU) |
  * @param header :  Header defined by the Modbus specification
  * @param pdu    :  Protocol data unit defined by the Modbus specification
  */
case class ADU(header: MBAP, pdu: PDU) {
  def toByteString: ByteString = header.toByteString ++ pdu.toByteString
}