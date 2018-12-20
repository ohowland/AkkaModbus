package modbus.frame

import akka.util.{ByteIterator, ByteString}
import modbus.template.WriteMultipleHoldingRegistersTemplate

trait WriteMultipleHoldingRegisters extends PDU
case object RequestWriteMultipleHoldingRegisters {
  val functionCode: Int = 0x10

  def apply(template: WriteMultipleHoldingRegistersTemplate): PDU = {
    val startAddress = template.startAddress
    val numberOfRegisters = template.numberOfRegisters
    val registerValues = List.empty
    RequestWriteMultipleHoldingRegisters(startAddress, numberOfRegisters, registerValues)
  }
}

case class RequestWriteMultipleHoldingRegisters(startAddress: Int, numberOfRegisters: Int, registerValues: List[Int])
  extends WriteMultipleHoldingRegisters {

  val payloadSize: Int = 2 * numberOfRegisters

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(RequestWriteMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.putByte(payloadSize.toByte)
    frameBuilder.putShorts(payload.map(_.toShort).toArray)
    frameBuilder.result()
  }

  override def length: Int = this.toByteString.length

  override def payload: List[Int] = registerValues
}

case object ResponseWriteMultipleHoldingRegisters {

  val functionCode: Int = 0x10
  def decode(in: ByteIterator) = {
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    val startAddress = in.getShort
    val registerCount = in.getShort
    ResponseWriteMultipleHoldingRegisters(startAddress, registerCount)
  }
}
case class ResponseWriteMultipleHoldingRegisters(startAddress: Int, numberOfRegisters: Int)
  extends WriteMultipleHoldingRegisters {

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ResponseWriteMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.result()
  }

  override def length: Int = this.toByteString.length
}

case object ExceptionWriteMultipleHoldingRegisters {
  val functionCode: Int = 0x90

  def decode(in: ByteIterator) = {
    val errorCode = in.getByte
    ExceptionWriteMultipleHoldingRegisters(errorCode)
  }
}
case class ExceptionWriteMultipleHoldingRegisters(errorCode: Int) extends WriteMultipleHoldingRegisters {

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ExceptionWriteMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putByte(errorCode.toByte)
    frameBuilder.result()
  }

  override def length: Int = this.toByteString.length
}