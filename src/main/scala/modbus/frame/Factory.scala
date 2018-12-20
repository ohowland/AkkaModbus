/**
  * FACTORY generates an ADU based on a Modbus template.
  *
  * NO BYTESTRINGS
  */

package modbus.frame

import modbus.template._

object Factory {
  def instanceOf(template: Template): ADU = {
    val pdu = template match {
      case template: ReadMultipleHoldingRegistersTemplate => RequestReadMultipleHoldingRegisters(template)
      case template: WriteMultipleHoldingRegistersTemplate => RequestWriteMultipleHoldingRegisters(template)
      case template: WriteSingleHoldingRegisterTemplate => RequestWriteSingleHoldingRegister(template)
    }

    val mbap = MBAP(0, pdu.length + 1, 0)
    ADU(mbap, pdu)
  }
}