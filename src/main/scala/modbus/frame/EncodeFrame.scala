package modbus.frame

import modbus.templates.{ModbusTemplate, ReadMultipleHoldingRegistersTemplate, WriteMultipleHoldingRegistersTemplate, WriteSingleHoldingRegisterTemplate}

object EncodeFrame {

  def encode(template: ModbusTemplate, transactionId: Int, unitId: Int, values: Int*): ADU = {
    val pdu = encodePDU(template, values)
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

  private def encodePDU(template: ModbusTemplate, values: Seq[Int]): PDU = {

    def encodeRequestReadMultipleHoldingRegisters(template: ReadMultipleHoldingRegistersTemplate) = {
      val startAddress = template.specification.startAddress
      val numberOfRegisters = template.specification.numberOfRegisters
      RequestReadHoldingRegisters(startAddress, numberOfRegisters)
    }

    def encodeRequestWriteMultipleHoldingRegisters(template: WriteMultipleHoldingRegistersTemplate) = {
      val startAddress = template.specification.startAddress
      val numberOfRegisters = template.specification.numberOfRegisters
      val registerValues = {
        if (values.isEmpty) None
        else Some(values.toList)
      }
      RequestWriteMultipleHoldingRegisters(startAddress, numberOfRegisters, registerValues)
    }

    def encodeRequestWriteSingleHoldingRegister(template: WriteSingleHoldingRegisterTemplate) = {
      val registerAddress = template.specification.startAddress
      val registerValue = values.headOption
      RequestWriteSingleHoldingRegister(registerAddress, registerValue)
    }

    val pdu: PDU = template match {
      case msgTemplate: ReadMultipleHoldingRegistersTemplate => encodeRequestReadMultipleHoldingRegisters(msgTemplate)
      case msgTemplate: WriteMultipleHoldingRegistersTemplate => encodeRequestWriteMultipleHoldingRegisters(msgTemplate)
      case msgTemplate: WriteSingleHoldingRegisterTemplate => encodeRequestWriteSingleHoldingRegister(msgTemplate)
      case _ => PDU.empty
    }
    pdu
  }
}
