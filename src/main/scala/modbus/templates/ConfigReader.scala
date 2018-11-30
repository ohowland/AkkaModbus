package modbus.templates

import java.net.URL
import scala.io.Source

object ConfigReader {
  def readAbsolutePath(path: URL): List[ModbusTypes.ModbusRegister] = {
    val bufferedSource = io.Source.fromURL(path)
    for {
      line <- bufferedSource.getLines
      Array(name, address, datatype, group, block) = line.split(",").map(_.trim)
      if toModbusDatatype(datatype).isDefined
    } yield ModbusTypes.ModbusRegister(name,
                                       address.toInt,
                                       toModbusDatatype(datatype).get,
                                       group,
                                       block.toInt)
  }.toList

  def readResource(name: String): List[ModbusTypes.ModbusRegister] = {
    val bufferedSource = io.Source.fromResource(name)
    for {
      line <- bufferedSource.getLines
      Array(name, address, datatype, group, block) = line.split(",").map(_.trim)
      if toModbusDatatype(datatype).isDefined
    } yield ModbusTypes.ModbusRegister(name,
      address.toInt,
      toModbusDatatype(datatype).get,
      group,
      block.toInt)
  }.toList

  def toModbusDatatype(datatype: String): Option[ModbusTypes.ModbusDatatype] = datatype match {
    case "U16" => Some(ModbusTypes.U16)
    case "U32" => Some(ModbusTypes.U32)
    case "I16" => Some(ModbusTypes.I16)
    case "I32" => Some(ModbusTypes.I32)
    case "F32" => Some(ModbusTypes.F32)
    case "F64" => Some(ModbusTypes.F64)
    case _ => None
  }
}
