package CGC.AssetCommunications

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object Read {
  def props(
             requestId: Long,
             targetActor: ActorRef,
             messageList: Set[MessageFactory.ModbusMessageTemplate],
             modbusMap: List[Comm.ModbusRegsiter],
             requester: ActorRef,
             timeout: FiniteDuration
  ): Props = Props(new Read(requestId, targetActor, messageList, modbusMap, requester, timeout))

  case object CollectionTimeout
}

class Read(
            requestId: Long,
            targetActor: ActorRef,
            messageList: Set[MessageFactory.ModbusMessageTemplate],
            modbusMap: List[Comm.ModbusRegsiter],
            requester: ActorRef,
            timeout: FiniteDuration
                     ) extends Actor with ActorLogging {

  import Read._
  import context.dispatcher

  val queryTimeoutTimer = context.system.scheduler.scheduleOnce(timeout, self, CollectionTimeout)
  var idToMessage: Map[Long, MessageFactory.ModbusMessageTemplate] = Map.empty

  override def preStart(): Unit = {
    context.watch(targetActor)

    messageList.foreach { messageTemplate =>
      val thisMessageId = Random.nextLong()
      val message = messageTemplate.build(thisMessageId)
      idToMessage = idToMessage + (thisMessageId -> messageTemplate)
      targetActor ! message
    }
  }

  override def postStop(): Unit = {
    queryTimeoutTimer.cancel()
  }

  override def receive: Receive = waitingForReplies(Map.empty, idToMessage.keySet)

  def waitingForReplies(
                         dataSoFar: Map[String, Double],
                         pendingMessageIds: Set[Long]
                       ): Receive = {
    case Comm.RespReadHoldingRegisters(messageId: Long, registers: Option[List[Int]]) =>
      val valueMap: Map[String, Double] = registers match {
        case Some(response) =>
          val template = idToMessage(messageId)
          template.decode(response)
        case None =>
          Map.empty
      }
      receivedResponse(messageId, valueMap, pendingMessageIds, dataSoFar)

    case Terminated(failedActor) =>
      requester ! Comm.ConnectionTimedOut(requestId)
      context.stop(self)

    case CollectionTimeout =>
      requester ! Comm.ConnectionTimedOut(requestId)
      context.stop(self)
  }

  def receivedResponse(
                        requestId:  Long,
                        valueMap: Map[String, Double],
                        pendingMessageIds: Set[Long],
                        dataSoFar: Map[String, Double]
                      ): Unit = {

    val newPendingMessageIds = pendingMessageIds - requestId

    val newDataSoFar = dataSoFar ++ valueMap
    if (newPendingMessageIds.isEmpty) {
      context.unwatch(targetActor)
      requester ! AssetActor.StatusMessage(requestId, newDataSoFar)
      context.stop(self)
    } else {
      context.become(waitingForReplies(newDataSoFar, newPendingMessageIds))
    }
  }
}

