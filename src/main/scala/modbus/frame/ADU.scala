/**
  * Modbus Application Data Unit
  */

package modbus.frame

import akka.util.ByteString
import modbus.templates.ModbusTemplate

case object ADU {
  def encode(template: ModbusTemplate, transactionId: Int, unitId: Int, values: Int*): ADU = {
    val pdu = PDU.encode(template, values)
    val payloadLength = pdu.length + 1
    ADU(MBAP.encode(transactionId, payloadLength, unitId), pdu) // the length of the message is the PDU length + unitId
  }

  def decode(data: ByteString): ADU = {
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN // <- TODO: I'm having trouble determining the location for this.

    ADU(MBAP.decode(data take 7), PDU.decode(data drop 7))
  }
}

case class ADU(mbap: MBAP, pdu: PDU) {
  def toByteString: ByteString = mbap.toByteString ++ pdu.toByteString
}

