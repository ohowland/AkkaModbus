package modbus.template

object Modbus {

  trait Datatype {
    val nWords: Int
  }
  case object U16 extends Datatype {
    val nWords: Int = 1
  }
  case object U32 extends Datatype {
    val nWords: Int = 2
  }
  case object I16 extends Datatype {
    val nWords: Int = 1
  }
  case object I32 extends Datatype {
    val nWords: Int = 2
  }
  case object F32 extends Datatype {
    val nWords: Int = 2
  }
  case object F64 extends Datatype {
    val nWords: Int = 4
  }

  case class Register(name: String,
                            address: Int,
                            datatype: Datatype,
                            group: String,
                            block: Int,
                            scale: Int)
}
