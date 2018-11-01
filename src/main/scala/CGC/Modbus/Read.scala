package CGC.Modbus

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object Read {
  def props(requestId: Long,
            targetActor: ActorRef,
            messages: Set[MessageFactory.ModbusMessageTemplate],
            requester: ActorRef,
            timeout: FiniteDuration
  ): Props = Props(new Read(requestId, targetActor, messages, requester, timeout))

  case object CollectionTimeout
  case class QueryResponse(requestId: Long, status: Map[String, Double])
}

class Read(requestId: Long,
           targetActor: ActorRef,
           messages: Set[MessageFactory.ModbusMessageTemplate],
           requester: ActorRef,
           timeout: FiniteDuration) extends Actor with ActorLogging {

  import Read._
  import context.dispatcher

  val queryTimeoutTimer = context.system.scheduler.scheduleOnce(timeout, self, CollectionTimeout)
  var idToMessageTemplate: Map[Long, MessageFactory.ModbusMessageTemplate] = Map.empty

  override def preStart(): Unit = {
    context.watch(targetActor)

    messages.foreach { messageTemplate =>
      val thisMessageId = Random.nextLong()
      val message = Comm.ReqReadHoldingRegisters(thisMessageId, 
                                                 messageTemplate.specification.startAddress,   
                                                 messageTemplate.specification.numberOfRegisters)
      idToMessageTemplate = idToMessageTemplate + (thisMessageId -> messageTemplate)
      targetActor ! message
    }
  }

  override def postStop(): Unit = {
    queryTimeoutTimer.cancel()
  }

  override def receive: Receive = waitingForReplies(Map.empty, idToMessageTemplate.keySet)

  def waitingForReplies(dataSoFar: Map[String, Double], pendingMessageIds: Set[Long]): Receive = {
    
    case Comm.RespReadHoldingRegisters(messageId: Long, registers: Option[List[Int]]) =>
      val incomingValueMap: Map[String, Double] = registers match {
        case None => Map.empty
        case Some(response) => idToMessageTemplate(messageId).decode(response)
      }
      receivedResponse(messageId, incomingValueMap, pendingMessageIds, dataSoFar)

    case Terminated(failedActor) =>
      //TODO: tell sender() the actor failed
      context.stop(self)

    case CollectionTimeout =>
      //TODO: tell sender() the actor timed out
      context.stop(self)
  }

  def receivedResponse(requestId:  Long,
                       incomingValueMap: Map[String, Double],
                       pendingMessageIds: Set[Long],
                       dataSoFar: Map[String, Double]): Unit = {

    val newPendingMessageIds = pendingMessageIds - requestId

    val newDataSoFar = dataSoFar ++ incomingValueMap
    if (newPendingMessageIds.isEmpty) {
      context.unwatch(targetActor)
      requester ! QueryResponse(requestId, newDataSoFar) // get rid of this dependency.
      context.stop(self)
    } else {
      context.become(waitingForReplies(newDataSoFar, newPendingMessageIds))
    }
  }
}

