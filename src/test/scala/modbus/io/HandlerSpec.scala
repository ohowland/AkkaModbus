package modbus.io

import java.net.InetSocketAddress

import modbus.frame._
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Terminated}
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
    "Buffer a RequestReadHoldingRegisters and transfer it to remote target" in {

      val remote = "localhost"
      val remoteName = "test-target-1"
      val bufferMaxSize = 10
      val config = HandlerConfig(remote, remoteName, bufferMaxSize)

      val pollProbe = TestProbe()
      val clientProbe = TestProbe()
      val clientHandlerActor = system.actorOf(Handler.props(config), "test-client-handler-1")

      val transactionId = util.Random.nextInt & 0xFF
      val unitId = 1 //util.Random.nextInt & 0x7F
      val testPDU = ByteString(0x03, 0 ,0 , 0, 1) // Read Multiple Registers starting at 0 spanning 1 word
      val testMBAP = ByteString((transactionId >> 8).toByte, transactionId.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      val testADU = testMBAP ++ testPDU

      clientHandlerActor.tell(Client.Write(testADU), pollProbe.ref)

      val response = pollProbe.expectMsgType[Client.Response]

      clientHandlerActor ! PoisonPill
    }

    "Buffer multiple RequestReadHoldingRegister and transfer them to remote target" in {

      val remote = "localhost"
      val remoteName = "test-target-2"
      val bufferMaxSize = 10
      val config = HandlerConfig(remote, remoteName, bufferMaxSize)

      val pollProbe = TestProbe()
      val clientProbe = TestProbe()
      val clientHandlerActor = system.actorOf(Handler.props(config), "test-client-handler-2")

      val transactionId1 = util.Random.nextInt & 0xFF
      val unitId = 1 //util.Random.nextInt & 0x7F
      var testPDU = ByteString(0x03, 0 ,0 , 0, 1) // Read Multiple Registers starting at 0 spanning 1 word
      var testMBAP = ByteString((transactionId1 >> 8).toByte, transactionId1.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      var testADU = testMBAP ++ testPDU
      clientHandlerActor.tell(Client.Write(testADU), pollProbe.ref)

      val transactionId2 = util.Random.nextInt & 0xFF
      testMBAP = ByteString((transactionId2 >> 8).toByte, transactionId2.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      testADU = testMBAP ++ testPDU
      clientHandlerActor.tell(Client.Write(testADU), pollProbe.ref)

      val transactionId3 = util.Random.nextInt & 0xFF
      testMBAP = ByteString((transactionId3 >> 8).toByte, transactionId3.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      testADU = testMBAP ++ testPDU
      clientHandlerActor.tell(Client.Write(testADU), pollProbe.ref)

      val transactionId4 = util.Random.nextInt & 0xFF
      testMBAP = ByteString((transactionId4 >> 8).toByte, transactionId4.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      testADU = testMBAP ++ testPDU
      clientHandlerActor.tell(Client.Write(testADU), pollProbe.ref)

      val transactionId5 = util.Random.nextInt & 0xFF
      testMBAP = ByteString((transactionId5 >> 8).toByte, transactionId5.toByte, 0, 0, 0, testPDU.length + 1, unitId)
      testADU = testMBAP ++ testPDU
      clientHandlerActor.tell(Client.Write(testADU), pollProbe.ref)

      val response1 = pollProbe.expectMsgType[Client.Response]
      val response2 = pollProbe.expectMsgType[Client.Response]
      val response3 = pollProbe.expectMsgType[Client.Response]
      val response4 = pollProbe.expectMsgType[Client.Response]
      val response5 = pollProbe.expectMsgType[Client.Response]

      clientHandlerActor ! PoisonPill
    }
  }
}
