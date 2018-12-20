/**
  * Modbus Application Data Unit
  */

package modbus.frame

import akka.util.ByteString

case object ADU {

  def decode(data: ByteString): ADU = {
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN // <- TODO: I'm having trouble determining the location for this.
    ADU(MBAP.decode(data take 7), PDU.decode(data drop 7))
  }
}

case class ADU(mbap: MBAP, pdu: PDU) {
  def toByteString: ByteString = mbap.toByteString ++ pdu.toByteString

  def setPayload(payload: List[Int]): ADU = {
    val newPdu = this.pdu.setPayload(payload)
    val newMbap = this.mbap.setLength(newPdu.length + 1)
    this.copy(mbap = newMbap, pdu = newPdu)
  }

  def setUnitId(id: Int): ADU = {
    val newMbap = this.mbap.setUnitId(id)
    this.copy(mbap = newMbap)
  }

  def setTransactionId(id: Int): ADU = {
    val newMbap = this.mbap.setTransactionId(id)
    this.copy(mbap = newMbap)
  }
}

