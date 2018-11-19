package modbus.io

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Tcp}
import java.net.InetSocketAddress
import akka.util.ByteString

/**
  * The io.Client is responsible for managing the TCP connection actor.
  *
  * Messages passed to the Client are passed to the TCP remote connection.
  * Messages recieved by the Client are passed to the io.Handler
  */
object Client {
  def props(remote: InetSocketAddress, listener: ActorRef) =
    Props(classOf[Client], remote, listener)

  case object ConnectionFailed
  case object WriteFailed
  case object ReqCloseConnection
  case class Write(data: ByteString)
  case class Response(data: ByteString)
}

class Client(remote: InetSocketAddress, handler: ActorRef)
  extends Actor
  with ActorLogging {

  import Client._
  import context.system // implicitly used by IO(Tcp)
  import akka.io.Tcp._

  IO(Tcp) ! Connect(remote)

  override def preStart(): Unit = log.info("Client started")
  override def postStop(): Unit = log.info("Client stopped")

  override def receive: Receive = {

    case CommandFailed(_: Connect) =>
      handler ! ConnectionFailed
      context stop self

    case c @ Connected(remote, local) =>
      handler ! c
      val connection = sender()
      connection ! Register(self, keepOpenOnPeerClosed = true)
      context become connected(connection)
  }

  def connected(connection: ActorRef): Receive = {

    case Client.Write(data) =>
      log.info(s"Client sending data: $data")
      connection ! Tcp.Write(data)

    case CommandFailed(w: Tcp.Write) =>
      log.info(s"O/S buffer was full")
      // O/S buffer was full
      handler ! WriteFailed

    case Received(data) =>
      // send data to handler
      log.info(s"Response ByteString $data")
      handler ! Response(data)

    case ReqCloseConnection =>
      connection ! Close

    case _: ConnectionClosed =>
      log.info(s"closed connection with [$connection]")
      context stop self

    case SuspendReading =>
      connection ! SuspendReading
  }

}
