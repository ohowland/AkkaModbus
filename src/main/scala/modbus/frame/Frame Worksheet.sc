import modbus.frame._
import modbus.templates.WriteSingleHoldingRegisterTemplate

val testTemplate = WriteSingleHoldingRegisterTemplate()

EncodeFrame.encode