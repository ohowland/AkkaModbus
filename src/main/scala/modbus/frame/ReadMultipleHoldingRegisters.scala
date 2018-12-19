package modbus.frame

import akka.util.{ByteIterator, ByteString}
import modbus.template.Template

trait ReadMultipleHoldingRegisters extends PDU

case object RequestReadMultipleHoldingRegisters {
  val functionCode: Int = 0x03

  def encode(template: Template) = {
    val startAddress = template.startAddress
    val numberOfRegisters = template.numberOfRegisters
    RequestReadMultipleHoldingRegisters(startAddress, numberOfRegisters)
  }
}
case class RequestReadMultipleHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends ReadMultipleHoldingRegisters {
  /**
    * Request Read Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |
    * DESCRIPTION : | Fct Code | Start Address | # Registers |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    frameBuilder.putByte(RequestReadMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putShort(startAddress)
    frameBuilder.putShort(numberOfRegisters)
    frameBuilder.result()
  }
  override def length: Int = {
    this.toByteString.length
  }
}

case object ResponseReadMultipleHoldingRegisters {
  val functionCode: Int = 0x03

  def decode(in: ByteIterator) = {
    val byteCount = in.getByte
    var shortArray: Array[Short] = Array.fill(byteCount / 2) {
      0
    }
    in.getShorts(shortArray, 0, byteCount / 2)
    val registerList: List[Int] = shortArray.map(_.toInt).toList
    ResponseReadMultipleHoldingRegisters(byteCount, registerList)
  }
}
case class ResponseReadMultipleHoldingRegisters(size: Int, response: List[Int]) extends ReadMultipleHoldingRegisters {
  /**
    * Response Read Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    1 Bytes    |   N Bytes   |
    * DESCRIPTION : | Fct Code |   Byte Count  |  Registers  |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    frameBuilder.putByte(ResponseReadMultipleHoldingRegisters.functionCode.toByte)
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

case object ExceptionReadMultipleHoldingRegisters {
  val functionCode: Int = 0x83

  def decode(in: ByteIterator) = {
    val error = in.getByte
    ExceptionReadMultipleHoldingRegisters(error)
  }
}
case class ExceptionReadMultipleHoldingRegisters(errorCode: Int) extends ReadMultipleHoldingRegisters {
  /**
    * Exception Read Holding Registers PDU Structure
    * BYTE        : |  1 Byte  |    1 Bytes    |
    * DESCRIPTION : | Fct Code |   Error Code  |
    */
  override def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    frameBuilder.putByte(ExceptionReadMultipleHoldingRegisters.functionCode.toByte)
    frameBuilder.putByte(errorCode.toByte)
    frameBuilder.result()
  }
  override def length: Int = this.toByteString.length
}



