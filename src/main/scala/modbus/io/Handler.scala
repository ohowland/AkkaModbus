package modbus.io

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.io.Tcp._
import akka.util.ByteString
import modbus.frame.ADU

import scala.collection.immutable.Queue

object Handler {
  def props(client: ActorRef, decoder: ActorRef): Props = Props(new Handler(client, decoder))

  case class Write(adu: ADU)
  case class Response(adu: ADU)

}

class Handler(client: ActorRef, decoder: ActorRef)
  extends Actor
  with ActorLogging {

  private var storage = List.empty[ADU]
  val maxSize: Int = 20

  import Handler._

  context watch client

  def receive: Receive = {
    case Write(adu) =>
      buffer(adu)
      client ! Client.Write(adu.toByteString)

      context.become({
        case Write(adu) => buffer(adu)
        case Received(data) => acknowledge(data)
        case Terminated(client) => context stop self
      })
  }

  private def buffer(adu: ADU): Unit = {
    storage ::: List(adu)

    if (storage.size > maxSize) {
      log.debug("dropping stale requests")
      storage = storage drop 1
    }
  }

  private def acknowledge(data: ByteString): Unit = {
    require(storage.nonEmpty, "storage was empty")

    storage = storage drop 1
    if (storage.isEmpty) {
      context.unbecome()
    } else client ! Client.Write(data)
  }

}
