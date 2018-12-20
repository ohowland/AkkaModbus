/**
  * TEMPLATES TURN MAP[STRING, DOUBLE] -> LIST[INT]
  * AND LIST[INT] -> MAP[STRING, DOUBLE]
  *
  */
package modbus.template

import scala.math.pow

case class ReadMultipleHoldingRegistersTemplate(specification: Template.BlockSpecification,
                                                registers: List[Modbus.Register],
                                                endianness: String) extends Template {

  override def encode(request: Map[String, Double]): List[Int] = ???

  override def decode(response: List[Int]): Map[String, Double] = {

      def buildDouble(words: List[Int], endianness: String): Double = {

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

      def sliceHoldingRegisters(response: List[Int], datatype: Modbus.Datatype, index: Int): List[Int] =
        response.slice(index, index + datatype.nWords)

      (for {
        register <- registers
        value = sliceHoldingRegisters(response, register.datatype, register.address - registers.head.address)
      } yield register.name -> buildDouble(value, endianness)).toMap
    }

  //Returns the total number of registers in the read request
  override def numberOfRegisters: Int = specification.numberOfRegisters

  override def startAddress: Int = specification.startAddress

}