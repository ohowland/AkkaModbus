package CGC.AssetCommunications

import akka.actor.{Actor, ActorLogging, Props}

object AssetActor {
  def props: Props = Props(new AssetActor)
  case class StatusMessage(requestId: Long, status: Map[String, Double])
}

class AssetActor extends Actor with ActorLogging {

  override def receive: Receive = ???
}
