package modbus.templates

import scala.math.pow

class ModbusTemplate {
  def numberOfRegisters: Int = 0
  def decode(response: List[Int]): Map[String, Double] = Map.empty
}

/**
  * Defines the data required for a modbus read multiple holding registers command.
  *
  * @param startAddress      is the start address for the modbus block read (inclusive)
  * @param numberOfRegisters is the number of registers to read, including the first.
  */
case class ReadSpecification(startAddress: Int, numberOfRegisters: Int)

/**
  * Defines the a template that contains the data required to create a modbus polling request, and decode the
  * returned data.
  *
  * @param specification data required to fulfill a Modbus read multiple holding registers command.
  * @param registers     a list of Modbus register classes which defines all holding registers in this message's domain.
  * @param endianness    the endianess of the message.
  */
case class ReadMultipleHoldingRegistersTemplate(specification: ReadSpecification,
                                                registers: List[ModbusTypes.ModbusRegister],
                                                endianness: String) extends ModbusTemplate {
  /**
    * Returns map of register name to register value as Map[String, Double].
    *
    * The decode method operates on the List[Int] returned by a read holding registers modbus call.
    * Messages created from a template are then decoded by the same template.
    *
    * @param response : A list of holding register values returned by a modbus poll
    */
  override def decode(response: List[Int]): Map[String, Double] = {

    /**
      * Returns a single Double type from a list of two byte values.
      *
      * The 2 byte 'words' are the Modbus standard holding register size. Words must be processed into
      * the correct type once read from the target device.
      *
      * @param words      List of 2 byte holding register values
      * @param endianness The order in which the list is interpreted. Two acceptible values: "big" and "little".
      */
    def buildDouble(words: List[Int], endianness: String): Double = {

      /**
        * Aggregates and returns the value of a Double by recusivly reading through a list of 2 byte words.
        *
        * @param words         is a list of 2 byte holding register values
        * @param word_position indexes current 2 bytes into their position in the double.
        * @param accumulator   accumulates the final value while recursing.
        * @return Double composed from a list of 2 byte words.
        */
      def buildDoubleIter(words: List[Int], word_position: Int, accumulator: Double): Double = {
        if (word_position >= words.size) accumulator
        else buildDoubleIter(words,
          word_position + 1,
          words(word_position).toDouble * pow(2, 16 * word_position) + accumulator)
      }

      val orderedWords = endianness match {
        case "big" => words.reverse
        case "little" => words
      }
      buildDoubleIter(orderedWords, 0, 0.0)
    }

    /**
      * Returns a subset list of the response parameter based on index and datatype size.
      *
      * @param response a list of 2 byte holding registers returned from a modbus read holding registers command.
      * @param datatype the target datatype
      * @param index    the start index in the response for the current
      * @return subset integer list of response
      */
    def sliceHoldingRegisters(response: List[Int], datatype: ModbusTypes.ModbusDatatype, index: Int): List[Int] =
      response.slice(index, index + ModbusTypes.modbusDatatypeWords(datatype))

    // the decode method takes each register, finds its location in the response list, and converts the
    // correct number of sequential words into a double
    (for {
      register <- registers
      value = sliceHoldingRegisters(response, register.datatype, register.address - registers.head.address)
    } yield register.name -> buildDouble(value, endianness)).toMap
  }

  /**
    * Returns the total number of registers in the read request
    */
  override def numberOfRegisters: Int = specification.numberOfRegisters
}

case class ReadSingleeHoldingRegisterTemplate(specification: ReadSpecification,
                                              registers: List[ModbusTypes.ModbusRegister],
                                              endianness: String) extends ModbusTemplate {
  /**
    * Returns map of register name to register value as Map[String, Double].
    *
    * The decode method operates on the List[Int] returned by a read holding registers modbus call.
    * Messages created from a template are then decoded by the same template.
    *
    * @param response : A list of holding register values returned by a modbus poll
    */
  override def decode(response: List[Int]): Map[String, Double] = {

    /**
      * Returns a single Double type from a list of two byte values.
      *
      * The 2 byte 'words' are the Modbus standard holding register size. Words must be processed into
      * the correct type once read from the target device.
      *
      * @param words      List of 2 byte holding register values
      * @param endianness The order in which the list is interpreted. Two acceptible values: "big" and "little".
      */
    def buildDouble(words: List[Int], endianness: String): Double = {

      /**
        * Aggregates and returns the value of a Double by recusivly reading through a list of 2 byte words.
        *
        * @param words         is a list of 2 byte holding register values
        * @param word_position indexes current 2 bytes into their position in the double.
        * @param accumulator   accumulates the final value while recursing.
        * @return Double composed from a list of 2 byte words.
        */
      def buildDoubleIter(words: List[Int], word_position: Int, accumulator: Double): Double = {
        if (word_position >= words.size) accumulator
        else buildDoubleIter(words,
          word_position + 1,
          words(word_position).toDouble * pow(2, 16 * word_position) + accumulator)
      }

      val orderedWords = endianness match {
        case "big" => words.reverse
        case "little" => words
      }
      buildDoubleIter(orderedWords, 0, 0.0)
    }

    /**
      * Returns a subset list of the response parameter based on index and datatype size.
      *
      * @param response a list of 2 byte holding registers returned from a modbus read holding registers command.
      * @param datatype the target datatype
      * @param index    the start index in the response for the current
      * @return subset integer list of response
      */
    def sliceHoldingRegisters(response: List[Int], datatype: ModbusTypes.ModbusDatatype, index: Int): List[Int] =
      response.slice(index, index + ModbusTypes.modbusDatatypeWords(datatype))

    // the decode method takes each register, finds its location in the response list, and converts the
    // correct number of sequential words into a double
    (for {
      register <- registers
      value = sliceHoldingRegisters(response, register.datatype, register.address - registers.head.address)
    } yield register.name -> buildDouble(value, endianness)).toMap
  }

  /**
    * Returns the total number of registers in the read request
    */
  override def numberOfRegisters: Int = specification.numberOfRegisters
}

/**
  * Defines the data required for a modbus write multiple holding registers command.
  *
  * @param startAddress      is the start address for the modbus block read (inclusive)
  * @param numberOfRegisters is the number of registers to read, including the first.
  */
case class WriteSpecification(startAddress: Int, numberOfRegisters: Int)

case class WriteMultipleHoldingRegistersTemplate(specification: WriteSpecification,
                                                 registers: List[ModbusTypes.ModbusRegister],
                                                 endianness: String) extends ModbusTemplate {

  /**
    * Returns map of register name to register value as Map[String, Double].
    *
    * The decode method operates on the List[Int] returned by a write holding registers Modbus call.
    * Messages created from a template are then decoded by the same template.
    *
    * @param response : A list of holding register values returned by a modbus poll
    */
  override def decode(response: List[Int]): Map[String, Double] = ???

  override def numberOfRegisters: Int = specification.numberOfRegisters
}

case class WriteSingleHoldingRegisterTemplate(specification: WriteSpecification,
                                              register: ModbusTypes.ModbusRegister,
                                              endianness: String) extends ModbusTemplate
