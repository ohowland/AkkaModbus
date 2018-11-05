package Modbus.IO

import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import java.net.InetSocketAddress

import Modbus.Frame.ADUFramer.ADUByteString

object Client {
  def props(remote: InetSocketAddress, listener: ActorRef) =
    Props(classOf[Client], remote, listener)

  case object ConnectionFailed
  case object WriteFailed
  case object ReqCloseConnection
  case class ReqWrite(adu: ByteString)
  case class RespWrite(adu: ByteString)
}

class Client(remote: InetSocketAddress, listener: ActorRef) extends Actor {

  import Client._
  import context.system // implicitly used by IO(Tcp)
  import Tcp._

  IO(Tcp) ! Connect(remote)

  override def receive: Receive = {
    case CommandFailed(_: Connect) =>
      listener ! ConnectionFailed
      context stop self

    case c @ Tcp.Connected(remote, local) =>
      listener ! c
      val connection = sender()
      connection ! Tcp.Register(self)
      context become connected(connection)
  }

  def connected(connection: ActorRef): Receive = {
    case ReqWrite(data) =>
      connection ! Write(data)

    case Tcp.CommandFailed(w: Write) =>
      // O/S buffer was full
      listener ! WriteFailed

    case Tcp.Received(data) =>
      listener ! RespWrite(data)

    case ReqCloseConnection =>
      connection ! Tcp.Close

    case c: Tcp.ConnectionClosed =>
      listener ! c
      context stop self
  }

}
