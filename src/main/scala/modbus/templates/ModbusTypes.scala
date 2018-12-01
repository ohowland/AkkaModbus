package modbus.templates

object ModbusTypes {

  trait ModbusDatatype
  case object U16 extends ModbusDatatype
  case object U32 extends ModbusDatatype
  case object I16 extends ModbusDatatype
  case object I32 extends ModbusDatatype
  case object F32 extends ModbusDatatype
  case object F64 extends ModbusDatatype

  /** converts the datatype to a word value (2 bytes), used in defining transaction lengths.
    *
    * @param datatype
    * @return
    */
  def modbusDatatypeWords(datatype: ModbusDatatype): Int = datatype match {
    case U16 => 1
    case U32 => 2
    case I16 => 1
    case I32 => 2
    case F32 => 2
    case F64 => 4
  }
  
  case class ModbusRegister(name: String,
                            address: Int,
                            datatype: ModbusDatatype,
                            group: String,
                            block: Int)
}
