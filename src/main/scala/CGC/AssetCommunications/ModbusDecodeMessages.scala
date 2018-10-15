package CGC.AssetCommunications

import scala.math.pow

object ModbusDecodeMessages {

  /**
    *
    * @param registers
    * @param endianness
    */
  case class DecodeRespReadHoldingRegisterTemplate(registers: List[ModbusCommActor.ModbusRegsiter], endianness: String)  {
    def decode(response: List[Short]): Map[String, Double] = {
      val startingAddress: Int = registers.head.address
      ( for {
        register <- registers
        index = register.address - startingAddress
        value: List[Short] = register.datatype match {
          case ModbusCommActor.U16 => List(response(index))
          case ModbusCommActor.I16 => List(response(index))
          case ModbusCommActor.U32 => List(response(index), response(index+1))
          case ModbusCommActor.I32 => List(response(index), response(index+1))
          case ModbusCommActor.F32 => List(response(index), response(index+1))
          case ModbusCommActor.F64 => List(response(index), response(index+1), response(index+2), response(index+3))
        }
      } yield register.name -> buildDouble(value, endianness)).toMap
    }
  }

  /**
    *
    * @param words
    * @param endianness
    * @return
    */
  def buildDouble(words: List[Short], endianness: String): Double = {

    def buildDoubleIter(words: List[Short], word_position: Int, aggregate: Double): Double = {
      if (word_position >= words.size) aggregate
      else buildDoubleIter(words, word_position + 1, words(word_position).toDouble * pow(2, 16 * word_position) + aggregate)
    }

    val orderedWords = endianness match {
      case "big" => words.reverse
      case "little" => words
    }
    buildDoubleIter(orderedWords, 0, 0.0)
  }

  /**
    *
    * @param requestTemplate
    * @param modbusMap
    * @param endianness
    * @return
    */
  def CreateRespHoldingRegisterMessageTemplate(requestTemplate: ModbusMessageFactory.ReadHoldingRegistersTemplate,
                                               modbusMap: List[ModbusCommActor.ModbusRegsiter],
                                               endianness: String):
  DecodeRespReadHoldingRegisterTemplate = {
    // Get registers within the block read
    val sortedRegisterList = modbusMap.filter(_.address >= requestTemplate.startAddress )
      .filter(_.address <= (requestTemplate.startAddress + requestTemplate.numberOfRegisters))
      .sortBy(_.address)

    DecodeRespReadHoldingRegisterTemplate(sortedRegisterList, endianness)
}

}
