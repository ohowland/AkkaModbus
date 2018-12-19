package modbus.frame

import akka.util.{ByteIterator, ByteString}
import modbus.template.WriteMultipleHoldingRegistersTemplate

trait WriteMultipleHoldingRegisters extends PDU
case object RequestWriteMultipleHoldingRegisters {
  val functionCode: Int = 0x10

  def encode(template: WriteMultipleHoldingRegistersTemplate, values: Map[String, Double]) = {
    val startAddress = template.startAddress
    val numberOfRegisters = template.numberOfRegisters
    val registerValues = template.encode(values)
    RequestWriteMultipleHoldingRegisters(startAddress, numberOfRegisters, registerValues)
  }
}

case class RequestWriteMultipleHoldingRegisters(startAddress: Int, numberOfRegisters: Int, registerValues: List[Int])
  extends WriteMultipleHoldingRegisters {

  val payloadSize: Int = 2 * numberOfRegisters

  /**
    * Request Write Multiple Holding Registers PDU Structure
    * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |        2 Bytes         | N Bytes |
    * DESCRIPTION : | Fct Code | Start Address | # Registers | Payload Size (N Bytes) | Payload |
    */
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
    val startAddress = in.getShort
    val registerCount = in.getShort
    ResponseWriteMultipleHoldingRegisters(startAddress, registerCount)
  }
}
case class ResponseWriteMultipleHoldingRegisters(startAddress: Int, numberOfRegisters: Int)
  extends WriteMultipleHoldingRegisters {

  /**
    * Response Write Multiple Holding Registers PDU Structure
    * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes    |
    * DESCRIPTION : | Fct Code | Start Address | # Registers  |
    */
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

  /**
    * Exception Write Multiple Holding Registers PDU Structure
    * BYTE        : |  1 Byte  |  1 Bytes   |
    * DESCRIPTION : | Fct Code | Error Code |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ExceptionWriteMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putByte(errorCode.toByte)
    frameBuilder.result()
  }

  override def length: Int = this.toByteString.length
}