/**
  * TEMPLATES TURN MAP[STRING, DOUBLE] -> LIST[INT]
  * AND LIST[INT] -> MAP[STRING, DOUBLE]
  *
  * NO PDUS
  */

package modbus.template
import modbus.frame.{PDU, RequestReadMultipleHoldingRegisters}

object Template {
  case class BlockSpecification(startAddress: Int, numberOfRegisters: Int)
}

trait Template {

  def numberOfRegisters: Int

  def startAddress: Int

  def decode(response: List[Int]): Map[String, Double] = Map.empty

  def encode(request: Map[String, Double]): List[Int] = List.empty

  def scale(value: Double, register: Modbus.Register): Double = value * register.scale

  def unscale(value: Double, register: Modbus.Register): Double = value / register.scale
}