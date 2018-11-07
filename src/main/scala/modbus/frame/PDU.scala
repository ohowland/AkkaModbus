package modbus.frame

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
  * --------------------------------------------------------------------
  * Read Multiple Holding Register request, response, and exception PDUs
  * --------------------------------------------------------------------
  */
trait ReadHoldingRegisters extends PDU

/**
  * Request Read Hold Registers PDU Structure
  * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |
  * DESCRIPTION : | Fct Code | Start Address | # Registers |
  */
case object RequestReadHoldingRegisters { val functionCode: Int = 0x03 }
case class RequestReadHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends ReadHoldingRegisters {

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(RequestReadHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.result()
  }

  override def length: Int = {
    this.toByteString.length
  }
}

/**
  * Response Read Hold Registers PDU Structure
  * BYTE        : |  1 Byte  |    1 Bytes    |   N Bytes   |
  * DESCRIPTION : | Fct Code |   Byte Count  |  Registers  |
  */
case object ResponseReadHoldingRegisters { val functionCode: Int = 0x03 }
case class ResponseReadHoldingRegisters(size: Int, response: List[Int]) extends ReadHoldingRegisters {

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ResponseReadHoldingRegisters.functionCode.toByte)
    frameBuilder.putByte(size.toByte)
    for (i <- response.indices) {
      frameBuilder.putShort(response(i))
    }
    frameBuilder.result()
  }

  override def length: Int = {
    this.toByteString.length
  }
}

/**
  * Exception Read Holding Registers PDU Structure
  * BYTE        : |  1 Byte  |    1 Bytes    |
  * DESCRIPTION : | Fct Code |   Error Code  |
  */
case object ExceptionReadHoldingRegisters { val functionCode: Int = 0x83 }
case class ExceptionReadHoldingRegisters(errorCode: Int) extends ReadHoldingRegisters {

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ExceptionReadHoldingRegisters.functionCode.toByte)
    frameBuilder.putByte(errorCode.toByte)
    frameBuilder.result()
  }

  override def length: Int = {
    this.toByteString.length
  }
}

/**
  * ---------------------------------------------------------------------
  * Write Multiple Holding Register request, response, and exception PDUs
  * ---------------------------------------------------------------------
  */
trait WriteHoldingRegisters extends PDU

/**
  * Write Hold Registers PDU Structure
  * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |        2 Bytes         | N Bytes |
  * DESCRIPTION : | Fct Code | Start Address | # Registers | Payload Size (N Bytes) | Payload |
  */
case object RequestWriteHoldingRegisters { val functionCode: Int = 0x10 }
case class RequestWriteHoldingRegisters(startAddress: Int, numberOfRegisters: Int, payload: List[Int])
  extends WriteHoldingRegisters {
  val payloadSize: Int = 2 * numberOfRegisters

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(RequestWriteHoldingRegisters.functionCode.toByte)
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

/**
  * Response Write Hold Registers PDU Structure
  * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes    |
  * DESCRIPTION : | Fct Code | Start Address | # Registers  |
  */
case object ResponseWriteHoldingRegisters { val functionCode: Int = 0x10 }
case class ResponseWriteHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends WriteHoldingRegisters {

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ResponseWriteHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.result()
}
  override def length: Int = {
    this.toByteString.length
  }
}

/**
  * Exception Write Holding Registers PDU Structure
  * BYTE        : |  1 Byte  |  1 Bytes   |
  * DESCRIPTION : | Fct Code | Error Code |
  */
case object ExceptionWriteHoldingRegisters { val functionCode: Int = 0x90 }
case class ExceptionWriteHoldingRegisters(errorCode: Int) extends WriteHoldingRegisters {

  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putByte(ExceptionWriteHoldingRegisters.functionCode.toByte)
    frameBuilder.putByte(errorCode.toByte)
    frameBuilder.result()
  }

  override def length: Int = {
    this.toByteString.length
  }
}