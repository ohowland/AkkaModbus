/**
  * Modbus Protocol Data Unit
  */

package modbus.frame

import akka.util.ByteString
import modbus.template._

object PDU {
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
    }
  }
}

class PDU {
  def setPayload(payload: List[Int]): PDU = this

  def toByteString: ByteString = ByteString.empty

  def length: Int = 0

  def payload: List[Int] = List.empty
}
