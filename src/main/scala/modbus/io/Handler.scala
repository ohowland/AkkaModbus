package modbus.io

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.io.Tcp._
import akka.util.ByteString
import modbus.frame.ADU

import scala.collection.immutable.Queue

/**
  * The io.Handler is responsible for buffering write and response messages.
  */
object Handler {
  def props(client: ActorRef, decoder: ActorRef): Props = Props(new Handler(client, decoder))

  case class Write(data: ByteString)
  case class Response(data: ByteString)

}

class Handler(client: ActorRef, decoder: ActorRef)
  extends Actor
  with ActorLogging {

  private var storage: List[ByteString] = List.empty
  val maxSize: Int = 20

  import Handler._

  context watch client

  def receive: Receive = {
    case Write(data) =>
      buffer(data)
      client ! Client.Write(data)

      context.become({
        case Write(data) => buffer(data)
        case Received(data) => acknowledge(data)
        case Terminated => context stop self
      })
  }

  private def buffer(data: ByteString): Unit = {
    data :: storage

    if (storage.size > maxSize) {
      log.debug("buffer full: ignoring incoming request")
      storage = storage drop 1
    }
  }

  private def acknowledge(data: ByteString): Unit = {
    require(storage.nonEmpty, "storage was empty")

    decoder ! data

    storage = storage drop 1
    if (storage.isEmpty) {
      context.unbecome()
    } else client ! Client.Write(storage.head)
  }

}
