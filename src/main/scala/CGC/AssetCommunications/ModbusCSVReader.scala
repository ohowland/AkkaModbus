package CGC.AssetCommunications

import java.net.URL

object ModbusCSVReader {
  def readCSVToRegisterList(path: URL): List[ModbusCommActor.ModbusRegsiter] = {
    val bufferedSource = io.Source.fromURL(path)
    for {
      line <- bufferedSource.getLines
      Array(name, address, datatype, access, group, block) = line.split(",").map(_.trim)
      if toModbusDatatype(datatype).isDefined && toModbusAccessType(access).isDefined
    } yield ModbusCommActor.ModbusRegsiter(name,
                                      address.toInt,
                                      toModbusDatatype(datatype).get,
                                      toModbusAccessType(access).get,
                                      group,
                                      block.toInt)
  }.toList



  def toModbusDatatype(datatype: String): Option[ModbusCommActor.ModbusDatatype] = datatype match {
    case "U16" => Some(ModbusCommActor.U16)
    case "U32" => Some(ModbusCommActor.U32)
    case "I16" => Some(ModbusCommActor.I16)
    case "I32" => Some(ModbusCommActor.I32)
    case "F32" => Some(ModbusCommActor.F32)
    case "F64" => Some(ModbusCommActor.F64)
    case _ => None
  }

  def toModbusAccessType(access: String): Option[ModbusCommActor.ModbusAccessType] = access.toLowerCase match {
    case "read" => Some(ModbusCommActor.Read)
    case "write" => Some(ModbusCommActor.Write)
    case "read/write" => Some(ModbusCommActor.ReadWrite)
  }
}