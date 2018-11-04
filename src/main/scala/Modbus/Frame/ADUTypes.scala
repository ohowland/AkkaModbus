package Modbus.Frame

import akka.util.ByteString

/**
  * See the official Modbus specification here:
  * http://www.modbus.org/docs/Modbus_Messaging_Implementation_Guide_V1_0b.pdf
  */

/**
  * MBAP Header
  *
  * @param transactionId : Identification of a MODBUS Request / Response transaction.
  * @param length        : Number of following bytes (i.e. the size of the PDU)
  * @param unitId        : Identification of a remote slave (aka Modbus ID)
  */
case class MBAP(transactionId: Int, length: Int, unitId: Int) {
  val protocolId: Int = 0 // always 0 for Modbus protocol

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
  *
  * @param header :  Header defined by the Modbus specification
  * @param pdu    :  Protocol data unit defined by the Modbus specification
  */
case class ADU(header: MBAP, pdu: PDU) {
  def toByteString: ByteString = header.toByteString ++ pdu.toByteString
}