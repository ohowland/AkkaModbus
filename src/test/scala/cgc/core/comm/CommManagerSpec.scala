package cgc.core.comm

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import cgc.core.comm.CommManager.UpdateStatus
import com.typesafe.config.{Config, ConfigFactory}
import modbus.poll.Poll.PollResponse
import org.scalatest._

class CommManagerSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("CommManagerSpec"))

  "A CommManager Actor" should {
    "launch a poll actor" in {
      val testDevice = TestProbe()
      val testConfig: Config = ConfigFactory.load().getConfig("cgc.assets.test1")
      val commManagerActor = system.actorOf(CommManager.props(testConfig, testDevice.ref))
      val response = testDevice.expectMsgType[UpdateStatus]

      response should===(UpdateStatus(Map(
        ("test1",0.0),
        ("test2",0.0),
        ("test3",0.0),
        ("test4",0.0),
        ("test5",0.0))))
    }
  }
}