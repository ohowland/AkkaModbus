package CGC.AssetCommunications

import java.net.URL

object ModbusMapReader {
  def readCSVToRegisterList(path: URL): List[Comm.ModbusRegsiter] = {
    val bufferedSource = io.Source.fromURL(path)
    for {
      line <- bufferedSource.getLines
      Array(name, address, datatype, access, group, block) = line.split(",").map(_.trim)
      if toModbusDatatype(datatype).isDefined && toModbusAccessType(access).isDefined
    } yield Comm.ModbusRegsiter(name,
                                      address.toInt,
                                      toModbusDatatype(datatype).get,
                                      toModbusAccessType(access).get,
                                      group,
                                      block.toInt)
  }.toList



  def toModbusDatatype(datatype: String): Option[Comm.ModbusDatatype] = datatype match {
    case "U16" => Some(Comm.U16)
    case "U32" => Some(Comm.U32)
    case "I16" => Some(Comm.I16)
    case "I32" => Some(Comm.I32)
    case "F32" => Some(Comm.F32)
    case "F64" => Some(Comm.F64)
    case _ => None
  }

  def toModbusAccessType(access: String): Option[Comm.ModbusAccessType] = access.toLowerCase match {
    case "read" => Some(Comm.Read)
    case "write" => Some(Comm.Write)
    case "read/write" => Some(Comm.ReadWrite)
  }
}