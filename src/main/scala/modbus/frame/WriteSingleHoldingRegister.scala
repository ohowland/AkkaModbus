package modbus.frame

import akka.util.{ByteIterator, ByteString}
import modbus.template._

trait WriteSingleHoldingRegister extends PDU

case object RequestWriteSingleHoldingRegister {
  val functionCode: Int = 0x06

  def apply(template: Template): PDU = {
    val registerAddress = template.startAddress
    val registerValue = List.empty
    RequestWriteSingleHoldingRegister(registerAddress, registerValue.head)
  }
}

case class RequestWriteSingleHoldingRegister(registerAddress: Int, registerValue: Int)
  extends WriteSingleHoldingRegister {
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(RequestWriteMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(registerAddress)
    frameBuilder.putShort(registerValue)
    frameBuilder.result()
  }
  override def length: Int = this.toByteString.length
  override def payload: List[Int] = List(registerValue)
  override def setPayload(payload: List[Int]): PDU = ???
}

case object ResponseWriteSingleHoldingRegister {
  val functionCode: Int = 0x06
  def decode(in: ByteIterator) = {
    val registerAddress = in.getShort
    val registerValue = in.getShort
    ResponseWriteSingleHoldingRegister(registerAddress, registerValue)
  }
}

case class ResponseWriteSingleHoldingRegister(registerAddress: Int, registerValue: Int)
  extends WriteSingleHoldingRegister {
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(RequestWriteMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(registerAddress)
    frameBuilder.putShort(registerValue)
    frameBuilder.result()
  }
  override def length: Int = this.toByteString.length
  override def payload: List[Int] = List(registerValue)
}

case object ExceptionWriteSingleHoldingRegister {
  val functionCode: Int = 0x86
  def decode(in: ByteIterator) = {
    val errorCode = in.getByte
    ExceptionWriteSingleHoldingRegister(errorCode)
  }
}

case class ExceptionWriteSingleHoldingRegister(errorCode: Int)
  extends WriteSingleHoldingRegister {
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ExceptionWriteSingleHoldingRegister.functionCode.toByte)
    frameBuilder.putByte(errorCode.toByte)
    frameBuilder.result()
  }
  override def length: Int = this.toByteString.length
}
