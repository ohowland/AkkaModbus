package modbus.template

import java.net.URL

object ConfigReader {
  def read(path: URL): List[Modbus.Register] = {
    val bufferedSource = io.Source.fromURL(path)
    for {
      line <- bufferedSource.getLines
      Array(name, address, datatype, group, block, scale) = line.split(",").map(_.trim)
      if toModbusDatatype(datatype).isDefined
    } yield Modbus.Register(
      name,
      address.toInt,
      toModbusDatatype(datatype).get,
      group,
      block.toInt,
      scale.toInt)
  }.toList

  def readResource(name: String): List[Modbus.Register] = {
    val bufferedSource = io.Source.fromResource(name)
    for {
      line <- bufferedSource.getLines
      Array(name, address, datatype, group, block, scale) = line.split(",").map(_.trim)
      if toModbusDatatype(datatype).isDefined
    } yield Modbus.Register(name,
      address.toInt,
      toModbusDatatype(datatype).get,
      group,
      block.toInt,
      scale.toInt)
  }.toList

  def toModbusDatatype(datatype: String): Option[Modbus.Datatype] = datatype match {
    case "U16" => Some(Modbus.U16)
    case "U32" => Some(Modbus.U32)
    case "I16" => Some(Modbus.I16)
    case "I32" => Some(Modbus.I32)
    case "F32" => Some(Modbus.F32)
    case "F64" => Some(Modbus.F64)
    case _ => None
  }
}
