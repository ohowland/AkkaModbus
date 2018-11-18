package modbus.io

import java.net.InetSocketAddress

import modbus.frame._
import akka.actor.{ActorRef, ActorSystem, Terminated}
import akka.io.Tcp
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import modbus.io.Handler.HandlerConfig
import org.scalatest._

class HandlerSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("HandlerSpec"))

  "A Client Handler Actor" should {
    "Buffer a request and transfer it to target" in {

      val remote = new InetSocketAddress("localhost", 502)
      val remoteName = "test-target"
      val bufferMaxSize = 10
      val config = HandlerConfig(remote, remoteName, bufferMaxSize)

      val pollProbe = TestProbe()
      val clientProbe = TestProbe()
      val clientHandlerActor = system.actorOf(Handler.props(config), "test-client-handler")

      val transactionId = util.Random.nextInt & 0xFF
      val unitId = 1 //util.Random.nextInt & 0x7F
      val testPDU = ByteString(0x03, 0 ,0 , 0, 1) // Read Multiple Registers starting at 0 spanning 1 word
      val testMBAP = ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      val testADU = testMBAP ++ testPDU

      clientHandlerActor.tell(Client.Write(testADU), pollProbe.ref)

      val response = pollProbe.expectMsgType[Client.Response]
    }
  }
}
