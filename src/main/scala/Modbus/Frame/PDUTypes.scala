package Modbus.Frame

import akka.util.ByteString

/**
  * See the official Modbus specification for PDU types here:
  * http://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf
  */
trait PDU {
  def toByteString: ByteString = ByteString.empty
  def length: Int = 0
}

object PDU extends PDU{
  val empty: PDU = PDU
}

/**
  * Read Multiple Holding Register request, response, and exception PDUs
  */
trait ReadHoldingRegisters extends PDU

/**
  * Read Holding Register Request PDU
  * @param startAddress      : Starting register for block read
  * @param numberOfRegisters : Number of consecutive registers to read
  */
case class RequestReadHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends ReadHoldingRegisters {
  val functionCode: Int = 0x03

/**
  * Read Hold Registers PDU Structure
  * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |
  * DESCRIPTION : | Fct Code | Start Address | # Registers |
  */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.result()
  }

  override def length: Int = {
    this.toByteString.length
  }
}
case class ResponseReadHoldingRegisters(size: Int, response: List[Int]) extends ReadHoldingRegisters {
  val functionCode: Int = 0x03
}
case class ExceptionReadHoldingRegisters(exceptionCode: Int) extends ReadHoldingRegisters {
  val functionCode: Int = 0x83
}

/**
  * Write Multiple Holding Register request, response, and exception PDUs
  */
trait WriteHoldingRegisters extends PDU

/**
  * Write Holding Registers Request PDU
  * @param startAddress      : Starting register for block write
  * @param numberOfRegisters : Number of consecutive registers to write
  * @param payload           : Data to be written
  */
case class RequestWriteHoldingRegisters(startAddress: Int, numberOfRegisters: Int, payload: List[Int])
  extends WriteHoldingRegisters {
  val functionCode: Int = 0x10
  val payloadSize: Int = 2 * numberOfRegisters

  /**
    * Write Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |        2 Bytes         | N Bytes |
    * DESCRIPTION : | Fct Code | Start Address | # Registers | Payload Size (N Bytes) | Payload |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.putByte(payloadSize.toByte)
    frameBuilder.putShorts(payload.map(_.toShort).toArray)
    frameBuilder.result()
  }

  override def length: Int = {
    this.toByteString.length
  }

}
case class ResponseWriteHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends WriteHoldingRegisters {
  val functionCode: Int = 0x10
}
case class ExceptionWriteHoldingRegisters(exceptionCode: Int) extends WriteHoldingRegisters {
  val functionCode: Int = 0x90
}