package modbus.frame

import akka.util.{ByteIterator, ByteString}

object DecodeFrame {

  implicit val byteOrder = java.nio.ByteOrder.BIG_ENDIAN

  def decode(data: ByteString): ADU = {
    ADU(decodeMBAP(data take 7), decodePDU(data drop 7))
  }

  /**
    * MBAP header structure:
    * BYTES :       |     2 Bytes    |   2 Bytes   |      2 Bytes       | 1 Byte  |
    * DESCRIPTION : | Transaction Id | Protocol Id | PDU Length (bytes) | Unit Id |
    * @return
    */
  private def decodeMBAP(data: ByteString): MBAP = {
    val in = data.iterator
    val transactionId = in.getShort
    val protocolId = in.getShort
    val length = in.getShort
    val unitId = in.getByte
    MBAP(transactionId, length, unitId)
  }

  private def decodePDU(data: ByteString): PDU = {

    def decodeResponseReadHoldingRegisters(in: ByteIterator) = {
      val byteCount = in.getByte
      var shortArray: Array[Short] = Array.fill(byteCount/2){ 0 }
      in.getShorts(shortArray, 0, byteCount/2)
      val registerList: List[Int] = shortArray.map(_.toInt).toList
      ResponseReadHoldingRegisters(byteCount, registerList)
    }

    def decodeExceptionReadHoldingRegisters(in: ByteIterator) = {
      val error = in.getByte
      ExceptionReadHoldingRegisters(error)
    }

    def decodeResponseWriteMultipleHoldingRegisters(in: ByteIterator) = {
      val startAddress = in.getShort
      val registerCount = in.getShort
      ResponseWriteMultipleHoldingRegisters(startAddress, registerCount)
    }

    def decodeExceptionWriteMultipleHoldingRegisters(in: ByteIterator) = {
      val errorCode = in.getByte
      ExceptionWriteMultipleHoldingRegisters(errorCode)
    }

    def decodeResponseWriteSingleRegisters(in: ByteIterator) = {
      val registerAddress = in.getShort
      val registerValue = in.getShort
      ResponseWriteSingleHoldingRegister(registerAddress, registerValue)
    }

    def decodeExceptionWriteSingleHoldingRegisters(in: ByteIterator) = {
      val errorCode = in.getByte
      ExceptionWriteSingleHoldingRegister(errorCode)
    }

    val in = data.iterator
    val functionCode: Int = in.getByte.toInt

    // 0xFF to get unsigned byte
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