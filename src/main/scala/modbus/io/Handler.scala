package modbus.io

import java.net.InetSocketAddress

import scala.collection.immutable.Queue
import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Terminated}
import Handler._
import akka.io.Tcp.Connected
import akka.util.ByteString
import modbus.io.Client.ReqCloseConnection

/**
  * Client Handler
  * 1. Has same message interface as the Client class
  * 2. Starts new client is previous has stopped and their are buffered messsages waiting to send
  * 3. Waits to send next message until it recieves a response from the Client.
  * 4. Routes returned messages to original sender.
  */

object Handler {
  def props(config: HandlerConfig): Props = Props(new Handler(config))

  case object CloseConnection
  case class HandlerConfig(remoteAddr: String, port: Int, remoteName: String, bufferMaxSize: Int)

}

class Handler(config: HandlerConfig) extends Actor with ActorLogging {

  var storage: Queue[(ByteString, ActorRef)] = Queue.empty
  val storageMaxSize: Int = config.bufferMaxSize

  override def preStart(): Unit = {log.info("ClientHandler started")}
  override def postStop(): Unit = {log.info("ClientHandler stopped")}

  /**
    * In the closed state, there is no data in the write buffer. Connection is closed.
    * @return
    */
  def receive: Receive = {
    case msg @ Client.Write(data) =>
      val remote = new InetSocketAddress(config.remoteAddr, config.port)
      val client = context.actorOf(Client.props(remote, context.self), config.remoteName)
      context.watch(client)
      buffer(data, sender())
      log.info("state transition: idle -> wait")
      context.become(wait(client))
  }

  /**
    * Waiting for connection ack.
    */
  def wait(client: ActorRef): Receive = {
    case msg @ Client.Write(data) =>
      buffer(data, sender())

    case Connected(remote, local) =>
      log.info("state transition: wait -> requesting")
      context.become(requesting(client))
      val msg = storage.dequeue._1._1
      log.info(s"sending client [$client] message [$msg]")
      client ! Client.Write(msg)

    case Terminated(child) =>
      log.info("state transition: wait -> stopped")
      storage = Queue.empty
      context.unbecome()
  }

  /**
    * In the idle state, there is no data in the write buffer. Connection is not maintained.
    */
  def idle(client: ActorRef): Receive = {
    case msg @ Client.Write(data) =>
      log.info("state transition: idle -> requesting")
      context.become(requesting(client))
      context.self forward msg
      // in the idle state, we need to transmit the first message. The buffer will take care of it from there.
      client ! msg

    case Terminated(child) =>
      log.info("state transition: idle -> stopped")
      context.unbecome()

    case CloseConnection =>
      client ! ReqCloseConnection
  }

  /**
    * In the requesting state, there is data in the write buffer. Connection is maintained.
    */
  def requesting(client: ActorRef): Receive = {
    case Client.Write(data) =>
      buffer(data, sender())

    case Client.Response(data) =>
      acknowledge(data, client)

    case Terminated(child) =>
      log.info("state transition: requesting -> stopped")
      storage = Queue.empty
      context.unbecome()

    case CloseConnection =>
      client ! ReqCloseConnection
  }

  def buffer(data: ByteString, sender: ActorRef): Unit = {
    if (storage.size <= storageMaxSize) storage = storage.enqueue((data, sender))
    log.info(s"storage: $storage")
    // ignore more incoming data if buffer is full.
  }

  def acknowledge(incomingData: ByteString, client: ActorRef): Unit = {
    val bufferedTuple: (ByteString, ActorRef) = storage.dequeue._1
    log.info(s"acknowledge: ${bufferedTuple._1} -> $incomingData")
    storage = storage.dequeue._2

    val route: ActorRef = bufferedTuple._2
    route ! incomingData

    if (storage.isEmpty) context.become(idle(client))
    else { val nextBufferedByteString = storage.dequeue._1._1
           client ! Client.Write(nextBufferedByteString) }
  }
}
