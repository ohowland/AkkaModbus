package modbus.frame

import akka.util.{ByteIterator, ByteString}
import modbus.poll.MessageFactory.{ModbusMessageTemplate, ReadModbusMessageTemplate, WriteModbusMessageTemplate}

object EncodeFrame {

  def encode(template: ModbusMessageTemplate, transactionId: Int, unitId: Int): ADU = {
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

  private def encodePDU(template: ModbusMessageTemplate): PDU = {

    def encodeRequestReadHoldingRegisters(template: ReadModbusMessageTemplate) = {
      val startAddress = template.specification.startAddress
      val numberOfRegisters = template.specification.numberOfRegisters
      RequestReadHoldingRegisters(startAddress, numberOfRegisters)
    }

    def encodeRequestWriteHoldingRegisters(template: ModbusMessageTemplate) = {
      val startAddress = ???
      val numberOfRegisters = ???
      val payload = ???
      RequestWriteHoldingRegisters(startAddress, numberOfRegisters, payload)
    }

    val pdu: PDU = template match {
      case msgTemplate: ReadModbusMessageTemplate => encodeRequestReadHoldingRegisters(msgTemplate)
      case msgTemplate: WriteModbusMessageTemplate => ???
      case _ => PDU.empty
    }
    pdu
  }
}
