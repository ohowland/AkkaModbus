package Modbus.Frame

import akka.actor.{ActorSystem, Actor, Props}
import akka.stream.{ActorMaterializer, IOResult}
import akka.stream.scaladsl.{FileIO, RunnableGraph, Sink, Source}
import akka.util.ByteString

/**
  * ADUFramer: convert Application Data Unit (ADU) to ByteString
  */
object ADUFramer {
  def props: Props = Props(new ADUFramer)

  case class ADUByteString(aduByteString: ByteString)
}

class ADUFramer extends Actor {

  import ADUFramer._

  def receive: Receive = {
    case adu: ADU =>
      sender() ! ADUByteString(adu.toByteString)
  }
}