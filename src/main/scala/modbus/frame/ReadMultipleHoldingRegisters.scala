package modbus.frame

import akka.util.{ByteIterator, ByteString}
import modbus.templates.ReadMultipleHoldingRegistersTemplate

trait ReadMultipleHoldingRegisters extends PDU

case object RequestReadHoldingRegisters {
  val functionCode: Int = 0x03

  def encode(template: ReadMultipleHoldingRegistersTemplate) = {
    val startAddress = template.specification.startAddress
    val numberOfRegisters = template.specification.numberOfRegisters
    RequestReadHoldingRegisters(startAddress, numberOfRegisters)
  }
}
case class RequestReadHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends ReadMultipleHoldingRegisters {
  /**
    * Request Read Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |
    * DESCRIPTION : | Fct Code | Start Address | # Registers |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    frameBuilder.putByte(RequestReadHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.result()
  }
  override def length: Int = {
    this.toByteString.length
  }
}

case object ResponseReadHoldingRegisters {
  val functionCode: Int = 0x03

  def decode(in: ByteIterator) = {
    val byteCount = in.getByte
    var shortArray: Array[Short] = Array.fill(byteCount / 2) {
      0
    }
    in.getShorts(shortArray, 0, byteCount / 2)
    val registerList: List[Int] = shortArray.map(_.toInt).toList
    ResponseReadHoldingRegisters(byteCount, registerList)
  }
}
case class ResponseReadHoldingRegisters(size: Int, response: List[Int]) extends ReadMultipleHoldingRegisters {
  /**
    * Response Read Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    1 Bytes    |   N Bytes   |
    * DESCRIPTION : | Fct Code |   Byte Count  |  Registers  |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
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

  override def payload: List[Int] = response
}

case object ExceptionReadHoldingRegisters {
  val functionCode: Int = 0x83

  def decode(in: ByteIterator) = {
    val error = in.getByte
    ExceptionReadHoldingRegisters(error)
  }
}
case class ExceptionReadHoldingRegisters(errorCode: Int) extends ReadMultipleHoldingRegisters {
  /**
    * Exception Read Holding Registers PDU Structure
    * BYTE        : |  1 Byte  |    1 Bytes    |
    * DESCRIPTION : | Fct Code |   Error Code  |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    frameBuilder.putByte(ExceptionReadHoldingRegisters.functionCode.toByte)
    frameBuilder.putByte(errorCode.toByte)
    frameBuilder.result()
  }
  override def length: Int = this.toByteString.length
}



