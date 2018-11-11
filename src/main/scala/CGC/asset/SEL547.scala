package CGC.asset

import akka.actor.{ Actor, ActorLogging, Props }

/** The Schweitzer Engineering Laboratories 547 (SEL547) is a protection relay used to enforce IEEE-547 at the point
  * of interconnection. This actor will spawn the proper communications Actor and provide it with the configuration
  * parameters required to complete the transaction. The purpose of modules at this level are to take the information
  * from the communications actor and map it to the Grid Intertie's actors data.
  */

object SEL547 {
  def props: Props = Props(new SEL547)

  sealed trait SEL547Message
  case class UpdateStatus(requestId: Long, statusMap: Map[String, Double]) extends SEL547Message
  case class WriteControl(requestId: Long) extends SEL547Message
}

class SEL547 extends Actor with ActorLogging {
  import SEL547._

  override def preStart(): Unit = log.info("SEL547 Actor started")
  override def postStop(): Unit = log.info("SEL547 Actor stopped")

  override def receive: Receive = {
    case UpdateStatus(id, statusMap) =>
      // update status should be triggered by the communications process when it has new information
      // this class maps the incoming data and sends a message to its parent

      val online: Boolean = statusMap.get("online") match {
        case Some(value) => value != 0
        case None => false
      }

      val kW: Double = statusMap.get("kW") match {
        case Some(value) => value
        case None => 0.0
      }

      val kVAR: Double = statusMap.get("kVAR") match {
        case Some(value) => value
        case None => 0.0
      }

      val newStatus = GridIntertie.Status(online, kW, kVAR)
      context.parent ! GridIntertie.ReqSetStatus(id, newStatus)
  }
}
