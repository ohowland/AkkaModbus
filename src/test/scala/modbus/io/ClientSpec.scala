package modbus.io

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.io.Tcp
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import scala.concurrent.duration._
import org.scalatest._

class ClientSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ClientSpec"))

  "A Modbus Client Actor" should {
    "establish a connection successfully" in {
      val testConfig: Config = ConfigFactory.load().getConfig("cgc.assets.testDevice1")
      val remote = new InetSocketAddress(
        testConfig.getString("communication.address"),
        testConfig.getInt("communication.port"))
      val probe = TestProbe()

      val clientActor = system.actorOf(Client.props(remote, probe.ref), "Test-Client")
      probe.watch(clientActor)
      probe.expectMsgType[Tcp.Connected]
      clientActor ! Client.ReqCloseConnection
      probe.expectTerminated(clientActor)
    }

    "transfer Modbus data to remote target" in {
      val testConfig: Config = ConfigFactory.load().getConfig("cgc.assets.testDevice1")
      val remote = new InetSocketAddress(
        testConfig.getString("communication.address"),
        testConfig.getInt("communication.port"))
      val probe = TestProbe()

      val clientActor = system.actorOf(Client.props(remote, probe.ref), "Test-Client")
      probe.watch(clientActor)
      probe.expectMsgType[Tcp.Connected]

      val transactionId = util.Random.nextInt & 0xFF
      val unitId = 1
      val testPDU = ByteString(0x03, 0 ,0 , 0, 1) // Read Multiple Registers starting at 0 spanning 1 word
      val testMBAP = ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      val testADU = testMBAP ++ testPDU
      clientActor ! Client.Write(testADU)
      val response = probe.expectMsgType[Client.Response]
      clientActor ! Client.ReqCloseConnection
      probe.expectTerminated(clientActor)
    }

    "report a failed connection attempt" in {
      val remote = new InetSocketAddress("bougusAddress", 502)
      val probe = TestProbe()
      val clientActor = system.actorOf(Client.props(remote, probe.ref), "Test-Client")
      probe.watch(clientActor)
      probe.expectMsg(10.seconds, Client.ConnectionFailed)
      probe.expectTerminated(clientActor)
    }
  }
}
