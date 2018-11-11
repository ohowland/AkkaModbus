package modbus.frame

import akka.util.ByteString

/**
  * See the official Modbus specification here:
  * http://www.modbus.org/docs/Modbus_Messaging_Implementation_Guide_V1_0b.pdf
  */

/**
  * Modbus ADU (Application Data Unit)
  *
  * @param mbap :  Header defined by the Modbus specification
  * @param pdu  :  Protocol data unit defined by the Modbus specification
  */
case class ADU(mbap: MBAP, pdu: PDU) {
  /**
    * ADU Structure:
    * BYTES:       |   7 Bytes   |         N Bytes          |
    * DESCRIPTION: | MBAP Header | Protocol Data Unit (PDU) |
    */
  def toByteString: ByteString = mbap.toByteString ++ pdu.toByteString
}

/**
  * MBAP Header for the Modbus ADU
  */
case object MBAP { val protocolId: Int = 0 }
/**
  * @param transactionId : Identification of a MODBUS Request / Response transaction.
  * @param length        : Number of following bytes (i.e. the size of the PDU)
  * @param unitId        : Identification of a remote slave (aka Modbus ID)
  */
case class MBAP(transactionId: Int, length: Int, unitId: Int) {
  /**
    * MBAP header structure:
    * BYTES :       |     2 Bytes    |   2 Bytes   |      2 Bytes       | 1 Byte  |
    * DESCRIPTION : | Transaction Id | Protocol Id | PDU Length (bytes) | Unit Id |
    * @return
    */
  def toByteString: ByteString = {
    val frameBuilder = ByteString.newBuilder
    implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    frameBuilder.putShort(transactionId)
    frameBuilder.putShort(MBAP.protocolId)
    frameBuilder.putShort(length)
    frameBuilder.putByte(unitId.toByte)
    frameBuilder.result()
  }
}

/**
  * See the official Modbus specification for PDU types here:
  * http://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf
  */

/**
  * Modbus PDU (Protocol Data Unit)
  */
trait PDU {
  def toByteString: ByteString = ByteString.empty
  def length: Int = 0
  def payload: List[Int] = List.empty
}
object PDU extends PDU{
  val empty: PDU = PDU
}

/*************************************************************************
  * Read Multiple Holding Register request, response, and exception PDUs *
  ************************************************************************/
trait ReadHoldingRegisters extends PDU

case object RequestReadHoldingRegisters { val functionCode: Int = 0x03 }
case class RequestReadHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends ReadHoldingRegisters {
  /**
    * Request Read Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |
    * DESCRIPTION : | Fct Code | Start Address | # Registers |
    */
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

case object ResponseReadHoldingRegisters { val functionCode: Int = 0x03 }
case class ResponseReadHoldingRegisters(size: Int, response: List[Int]) extends ReadHoldingRegisters {
  /**
    * Response Read Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    1 Bytes    |   N Bytes   |
    * DESCRIPTION : | Fct Code |   Byte Count  |  Registers  |
    */
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

  override def payload: List[Int] = response
}

case object ExceptionReadHoldingRegisters { val functionCode: Int = 0x83 }
case class ExceptionReadHoldingRegisters(errorCode: Int) extends ReadHoldingRegisters {
  /**
    * Exception Read Holding Registers PDU Structure
    * BYTE        : |  1 Byte  |    1 Bytes    |
    * DESCRIPTION : | Fct Code |   Error Code  |
    */
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

/**************************************************************************
  * Write Multiple Holding Register request, response, and exception PDUs *
  *************************************************************************/
trait WriteHoldingRegisters extends PDU

/**
  * Write Hold Registers PDU Structure
  * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes   |        2 Bytes         | N Bytes |
  * DESCRIPTION : | Fct Code | Start Address | # Registers | Payload Size (N Bytes) | Payload |
  */
case object RequestWriteHoldingRegisters { val functionCode: Int = 0x10 }
case class RequestWriteHoldingRegisters(startAddress: Int, numberOfRegisters: Int, registers: List[Int])
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

  override def payload = registers
}

case object ResponseWriteHoldingRegisters { val functionCode: Int = 0x10 }
case class ResponseWriteHoldingRegisters(startAddress: Int, numberOfRegisters: Int) extends WriteHoldingRegisters {
  /**
    * Response Write Hold Registers PDU Structure
    * BYTE        : |  1 Byte  |    2 Bytes    |   2 Bytes    |
    * DESCRIPTION : | Fct Code | Start Address | # Registers  |
    */
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

case object ExceptionWriteHoldingRegisters { val functionCode: Int = 0x90 }
case class ExceptionWriteHoldingRegisters(errorCode: Int) extends WriteHoldingRegisters {
  /**
    * Exception Write Holding Registers PDU Structure
    * BYTE        : |  1 Byte  |  1 Bytes   |
    * DESCRIPTION : | Fct Code | Error Code |
    */
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