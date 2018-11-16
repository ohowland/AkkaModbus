package modbus.io

import java.net.InetSocketAddress

import modbus.frame._
import akka.actor.{ActorSystem, ActorRef, Terminated}
import akka.io.Tcp
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import org.scalatest._

class ClientSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ClientSpec"))

  "A Modbus Client Actor" should {
    "establish a connection successfully" in {

      val remote = new InetSocketAddress("localhost", 502)
      val probe = TestProbe()

      val clientActor = system.actorOf(Client.props(remote, probe.ref), "Test-Client")
      probe.watch(clientActor)
      probe.expectMsgType[Tcp.Connected]
      clientActor ! Client.ReqCloseConnection
      probe.expectTerminated(clientActor)
    }

    "Transfer Modbus data to remote target" in {

      val remote = new InetSocketAddress("localhost", 502)
      val probe = TestProbe()

      val clientActor = system.actorOf(Client.props(remote, probe.ref), "Test-Client")
      probe.watch(clientActor)
      probe.expectMsgType[Tcp.Connected]

      val transactionId = 1
      val unitId = 1
      val testPDU = ByteString(0x03, 0 ,0 , 0, 1) // Read Multiple Registers starting at 0 spanning 1 word
      val testMBAP = ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId)

      val testADU = ByteString(0, transactionId, 0, 0, 0, testPDU.length + 1, unitId) ++ testPDU
      clientActor ! Client.Write(testADU)
      val response = probe.expectMsgType[Client.Response]

      clientActor ! Client.ReqCloseConnection
      probe.expectTerminated(clientActor)
    }
  }
}
