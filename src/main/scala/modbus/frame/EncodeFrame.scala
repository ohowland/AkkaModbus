package modbus.frame

import modbus.templates.{ModbusTemplate, ReadMultipleHoldingRegistersTemplate, WriteMultipleHoldingRegistersTemplate}

object EncodeFrame {

  def encode(template: ModbusTemplate, transactionId: Int, unitId: Int): ADU = {
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

  private def encodePDU(template: ModbusTemplate): PDU = {

    def encodeRequestReadMultipleHoldingRegisters(template: ReadMultipleHoldingRegistersTemplate) = {
      val startAddress = template.specification.startAddress
      val numberOfRegisters = template.specification.numberOfRegisters
      RequestReadHoldingRegisters(startAddress, numberOfRegisters)
    }

    def encodeRequestWriteMultipleHoldingRegister(template: WriteMultipleHoldingRegistersTemplate) = {
      val startAddress = template.specification.startAddress
      val numberOfRegisters = template.specification.numberOfRegisters
      val payload = ???
      RequestWriteHoldingRegisters(startAddress, numberOfRegisters, payload)
    }

    val pdu: PDU = template match {
      case msgTemplate: ReadMultipleHoldingRegistersTemplate => encodeRequestReadMultipleHoldingRegisters(msgTemplate)
      case msgTemplate: WriteMultipleHoldingRegistersTemplate => encodeRequestWriteMultipleHoldingRegister(msgTemplate)
      case _ => PDU.empty
    }
    pdu
  }
}
