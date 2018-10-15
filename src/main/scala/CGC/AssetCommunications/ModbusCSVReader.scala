package CGC.AssetCommunications

import java.net.URL

object ModbusCSVReader {
  def readCSVToRegisterList(path: URL): List[ModbusComm.ModbusRegsiter] = {
    val bufferedSource = io.Source.fromURL(path)
    for {
      line <- bufferedSource.getLines
      Array(name, address, datatype, access, group, block) = line.split(",").map(_.trim)
      if toModbusDatatype(datatype).isDefined && toModbusAccessType(access).isDefined
    } yield ModbusComm.ModbusRegsiter(name,
                                      address.toInt,
                                      toModbusDatatype(datatype).get,
                                      toModbusAccessType(access).get,
                                      group,
                                      block.toInt)
  }.toList



  def toModbusDatatype(datatype: String): Option[ModbusComm.ModbusDatatype] = datatype match {
    case "U16" => Some(ModbusComm.U16)
    case "U32" => Some(ModbusComm.U32)
    case "I16" => Some(ModbusComm.I16)
    case "I32" => Some(ModbusComm.I32)
    case "F32" => Some(ModbusComm.F32)
    case "F64" => Some(ModbusComm.F64)
    case _ => None
  }

  def toModbusAccessType(access: String): Option[ModbusComm.ModbusAccessType] = access.toLowerCase match {
    case "read" => Some(ModbusComm.Read)
    case "write" => Some(ModbusComm.Write)
    case "read/write" => Some(ModbusComm.ReadWrite)
  }
}