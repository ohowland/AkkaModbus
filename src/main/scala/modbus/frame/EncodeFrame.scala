package modbus.frame

import akka.util.{ByteIterator, ByteString}
import modbus.templates.Factory.{MessageTemplate, ReadModbusMessageTemplate, WriteModbusMessageTemplate}

object EncodeFrame {

  def encode(template: MessageTemplate, transactionId: Int, unitId: Int): ADU = {
    val pdu = encodePDU(template)
    ADU(encodeMBAP(transactionId, pdu.length + 1, unitId), pdu) // the length of the message is the PDU length + unitId
  }

  /**
    * MBAP header structure:
    * BYTES :       |     2 Bytes    |   2 Bytes   |      2 Bytes       | 1 Byte  |
    * DESCRIPTION : | Transaction Id | Protocol Id | PDU Length (bytes) | Unit Id |
    * @return
    */
  private def encodeMBAP(transactionId: Int, length: Int, unitId: Int): MBAP = {
    MBAP(transactionId, length, unitId)
  }

  private def encodePDU(template: MessageTemplate): PDU = {

    def encodeRequestReadHoldingRegisters(template: ReadModbusMessageTemplate) = {
      val startAddress = template.specification.startAddress
      val numberOfRegisters = template.specification.numberOfRegisters
      RequestReadHoldingRegisters(startAddress, numberOfRegisters)
    }

    def encodeRequestWriteHoldingRegisters(template: MessageTemplate) = {
      val startAddress = ???
      val numberOfRegisters = ???
      val payload = ???
      RequestWriteHoldingRegisters(startAddress, numberOfRegisters, payload)
    }

    val pdu: PDU = template match {
      case msgTemplate: ReadModbusMessageTemplate => encodeRequestReadHoldingRegisters(msgTemplate)
      case msgTemplate: WriteModbusMessageTemplate => encodeRequestWriteHoldingRegisters(msgTemplate)
      case _ => PDU.empty
    }
    pdu
  }
}
