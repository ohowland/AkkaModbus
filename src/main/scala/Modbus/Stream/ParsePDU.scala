package Modbus.Stream

import akka.actor.{ActorSystem, Actor, Props}
import akka.stream.{ActorMaterializer, IOResult}
import akka.stream.scaladsl.{FileIO, RunnableGraph, Sink, Source}
import akka.util.ByteString
import akka.io.{ IO, Tcp }

object ParsePDU {
  def props: Props = Props(new ParsePDU)
}

class ParsePDU extends Actor{
  import context.system

  val manager = IO(Tcp)
  def receive: Receive = ???
}
