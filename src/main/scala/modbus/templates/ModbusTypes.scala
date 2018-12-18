package modbus.templates

object ModbusTypes {

  trait ModbusDatatype {
    def byteSize: Int
  }
  case object U16 extends ModbusDatatype {
    def byteSize: Int = 1
  }
  case object U32 extends ModbusDatatype {
    def byteSize: Int = 2
  }
  case object I16 extends ModbusDatatype {
    def byteSize: Int = 1
  }
  case object I32 extends ModbusDatatype {
    def byteSize: Int = 2
  }
  case object F32 extends ModbusDatatype {
    def byteSize: Int = 2
  }
  case object F64 extends ModbusDatatype {
    def byteSize: Int = 4
  }

  case class ModbusRegister(name: String,
                            address: Int,
                            datatype: ModbusDatatype,
                            group: String,
                            block: Int,
                            scale: Int)
}
