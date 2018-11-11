package modbus.poll

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.util.ByteString
import modbus.frame.{DecodeFrame, EncodeFrame}

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/**
  * Poll is initialized with a set of modbus messages, and a target message handler.
  * The messages are mapped to a transactionId, and then encoded and sent to the
  * Modbus Client Handler
  */

object Poll {
  def props(requestId: Long,
            clientHandler: ActorRef,
            messages: Set[MessageFactory.ModbusMessageTemplate],
            unitId: Int,
            requester: ActorRef,
            timeout: FiniteDuration
           ): Props = Props(new Poll(requestId, clientHandler, messages, unitId, requester, timeout))

  case object CollectionTimeout
  case class PollResponse(requestId: Long, namedValueMap: Map[String, Double])
}

class Poll(requestId: Long,
           clientHandler: ActorRef,
           messages: Set[MessageFactory.ModbusMessageTemplate],
           unitId: Int,
           requester: ActorRef,
           timeout: FiniteDuration) extends Actor with ActorLogging {

  import Poll._
  import context.dispatcher

  val queryTimeoutTimer = context.system.scheduler.scheduleOnce(timeout, self, CollectionTimeout)
  var idToMessageTemplate: Map[Int, MessageFactory.ModbusMessageTemplate] = Map.empty

  override def preStart(): Unit = {
    context.watch(clientHandler)

    messages.foreach { messageTemplate =>
      val transactionId = Random.nextInt(65535) // Cap at maximum size U16
      val messageADU = EncodeFrame.encode(messageTemplate, transactionId, unitId)
      idToMessageTemplate = idToMessageTemplate + (transactionId -> messageTemplate)
      clientHandler ! messageADU
    }
  }

  override def postStop(): Unit = {
    queryTimeoutTimer.cancel()
  }

  override def receive: Receive = waitingForReplies(Map.empty, idToMessageTemplate.keySet)

  def waitingForReplies(dataSoFar: Map[String, Double], pendingTransactions: Set[Int]): Receive = {

    case data: ByteString =>
      val adu = DecodeFrame.decode(data)
      val incomingValueMap = idToMessageTemplate(adu.mbap.transactionId).decode(adu.pdu.payload)
      receivedResponse(adu.mbap.transactionId, incomingValueMap, pendingTransactions, dataSoFar)

    case Terminated(failedActor) =>
      //TODO: tell sender() the actor failed
      context.stop(self)

    case CollectionTimeout =>
      //TODO: tell sender() the actor timed out
      context.stop(self)
  }

  def receivedResponse(transactionId: Int,
                       incomingValueMap: Map[String, Double],
                       pendingTransactions: Set[Int],
                       dataSoFar: Map[String, Double]): Unit = {

    val newPendingTransactions = pendingTransactions - transactionId

    val newDataSoFar = dataSoFar ++ incomingValueMap
    if (newPendingTransactions.isEmpty) {
      context.unwatch(clientHandler)
      requester ! PollResponse(requestId, newDataSoFar) // get rid of this dependency.
      context.stop(self)
    } else {
      context.become(waitingForReplies(newDataSoFar, newPendingTransactions))
    }
  }
}

