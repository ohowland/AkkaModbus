package CGC.AssetModels

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest._

class ModbusCommSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ModbusCommSpec"))

  "A Modbus Communication Actor" should {
    "" in {
      val probe = TestProbe()
      val gridActor = system.actorOf(GridIntertie.props)

}
