/**
  * Modbus Protocol Data Unit
  */

package modbus.frame

import akka.util.ByteString
import modbus.template._

object PDU {
  val empty = new PDU

  def encode(template: Template, values: Map[String, Double]): PDU = {
    template match {
      case t: ReadMultipleHoldingRegistersTemplate => RequestReadMultipleHoldingRegisters.encode(t)
      case t: WriteMultipleHoldingRegistersTemplate => RequestWriteMultipleHoldingRegisters.encode(t, values)
      case t: WriteSingleHoldingRegisterTemplate => RequestWriteSingleHoldingRegister.encode(t, values)
      case _ => empty
    }
  }

  def decode(data: ByteString): PDU = {
    val in = data.iterator
    val functionCode: Int = in.getByte.toInt

    // 0xFF to get unsigned byte TODO: Function code as byte instead of integer.
    functionCode & 0xFF match {
      case ResponseReadMultipleHoldingRegisters.functionCode => ResponseReadMultipleHoldingRegisters.decode(in)
      case ResponseWriteSingleHoldingRegister.functionCode => ResponseWriteSingleHoldingRegister.decode(in)
      case ResponseWriteMultipleHoldingRegisters.functionCode => ResponseWriteMultipleHoldingRegisters.decode(in)
      case ExceptionReadMultipleHoldingRegisters.functionCode => ExceptionReadMultipleHoldingRegisters.decode(in)
      case ExceptionWriteSingleHoldingRegister.functionCode => ExceptionWriteSingleHoldingRegister.decode(in)
      case ExceptionWriteMultipleHoldingRegisters.functionCode => ExceptionWriteMultipleHoldingRegisters.decode(in)
      case _ => empty
    }
  }
}

class PDU {
  def toByteString: ByteString = ByteString.empty
  def length: Int = 0
  def payload: List[Int] = List.empty
}
