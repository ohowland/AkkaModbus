package CGC.Modbus

import org.scalatest.{BeforeAndAfterAll, MustMatchers, Suite, WordSpecLike}
import akka.actor.ActorSystem
import akka.testkit.TestKit

trait StopSystemAfterAll extends BeforeAndAfterAll {
  this: TestKit with Suite =>
  override protected def afterAll() = {
    super.afterAll()
    //system.shutdown()
  }
}

class MessageFactorySpec extends TestKit(ActorSystem("MessageFactorySpec"))
  with WordSpecLike
  with MustMatchers
  with StopSystemAfterAll {

  "A Message Factory" must {
    "something" in {
      succeed
    }
  }
}
