/**
  * TEMPLATES TURN MAP[STRING, DOUBLE] -> LIST[INT]
  * AND LIST[INT] -> MAP[STRING, DOUBLE]
  *
  * NO PDUS
  */
package modbus.template

import scala.math.pow

class ReadMultipleHoldingRegistersTemplate(specification: Template.BlockSpecification,
                                           registers: List[Modbus.Register],
                                           endianness: String) extends Template {

  override def encode(request: Map[String, Double]): List[Int] = ???

  // Returns map of register name to register value as Map[String, Double].
  override def decode(response: List[Int]): Map[String, Double] = {

      // Returns a single Double type from a list of two byte values.
      def buildDouble(words: List[Int], endianness: String): Double = {


        // Aggregates and returns the value of a Double by recusivly reading through a list of 2 byte words.
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
      def sliceHoldingRegisters(response: List[Int], datatype: Modbus.Datatype, index: Int): List[Int] =
        response.slice(index, index + datatype.nWords)

      // the decode method takes each register, finds its location in the response list, and converts the
      // correct number of sequential words into a double
      (for {
        register <- registers
        value = sliceHoldingRegisters(response, register.datatype, register.address - registers.head.address)
      } yield register.name -> buildDouble(value, endianness)).toMap
    }

  //Returns the total number of registers in the read request
  override def numberOfRegisters: Int = specification.numberOfRegisters

  override def startAddress: Int = specification.startAddress

}