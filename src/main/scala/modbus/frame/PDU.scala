/**
  * Modbus Protocol Data Unit
  */

package modbus.frame

import akka.util.ByteString
import modbus.frame.DecodeFrame._
import modbus.templates.ModbusTemplate

object PDU {
  val empty = new PDU

  def encode(template: ModbusTemplate, values: Seq[Int]): PDU = {
    template match {
      case msgTemplate: ReadMultipleHoldingRegistersTemplate => encodeRequestReadMultipleHoldingRegisters(msgTemplate)
      case msgTemplate: WriteMultipleHoldingRegistersTemplate => encodeRequestWriteMultipleHoldingRegisters(msgTemplate)
      case msgTemplate: WriteSingleHoldingRegisterTemplate => encodeRequestWriteSingleHoldingRegister(msgTemplate)
      case _ => PDU.empty
    }
  }

  def decode(data: ByteString): PDU = {
    val in = data.iterator
    val functionCode: Int = in.getByte.toInt

    // 0xFF to get unsigned byte TODO: Function code as byte instead of integer.
    functionCode & 0xFF match {
      case ResponseReadHoldingRegisters.functionCode => decodeResponseReadHoldingRegisters(in)
      case ExceptionReadHoldingRegisters.functionCode => decodeExceptionReadHoldingRegisters(in)
      case ResponseWriteSingleHoldingRegister.functionCode => decodeResponseWriteSingleRegisters(in)
      case ExceptionWriteSingleHoldingRegister.functionCode => decodeExceptionWriteSingleHoldingRegisters(in)
      case ResponseWriteMultipleHoldingRegisters.functionCode => decodeResponseWriteMultipleHoldingRegisters(in)
      case ExceptionWriteMultipleHoldingRegisters.functionCode => decodeExceptionWriteMultipleHoldingRegisters(in)
      case _ => PDU.empty
    }
  }
}

class PDU {
  def toByteString: ByteString = ByteString.empty
  def length: Int = 0
  def payload: List[Int] = List.empty
}
