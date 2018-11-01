import scala.concurrent._
import scala.util._
import ExecutionContext.Implicits.global

val futureFail = Future { throw new Exception("error") }

futureFail.onFailure {
  case e => println(e)
}