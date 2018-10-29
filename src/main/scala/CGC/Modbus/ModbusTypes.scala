package CGC.Modbus

object ModbusTypes {

  trait ModbusDatatype
  case object U16 extends ModbusDatatype
  case object U32 extends ModbusDatatype
  case object I16 extends ModbusDatatype
  case object I32 extends ModbusDatatype
  case object F32 extends ModbusDatatype
  case object F64 extends ModbusDatatype
  
  case class ModbusRegsiter(name: String,
                            address: Int,
                            datatype: ModbusDatatype,
                            group: String,
                            block: Int)
}
