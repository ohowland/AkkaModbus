package modbus.poll

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.util.ByteString
import modbus.io.Client.Write
import modbus.template.Template
import modbus.frame.{ADU, Factory}

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
            templates: Set[Template],
            unitId: Int,
            requester: ActorRef,
            timeout: FiniteDuration
           ): Props = Props(new Poll(requestId, clientHandler, templates, unitId, requester, timeout))

  case object CollectionTimeout
  case class PollResponse(requestId: Long, namedValueMap: Map[String, Double])
}

class Poll(requestId: Long,
           clientHandler: ActorRef,
           templates: Set[Template],
           unitId: Int,
           requester: ActorRef,
           timeout: FiniteDuration) extends Actor with ActorLogging {

  import Poll._
  import context.dispatcher

  val queryTimeoutTimer = context.system.scheduler.scheduleOnce(timeout, self, CollectionTimeout)

  override def preStart(): Unit = {
    context.watch(clientHandler)

    val idToMessageTemplate = {for {
      template <- templates
      transactionId = Random.nextInt(65535) & 0xFF
    } yield transactionId -> template}.toMap
    self ! idToMessageTemplate

    for ((transactionId, template) <- idToMessageTemplate) {
      val adu = Factory.instanceOf(template)
        .setTransactionId(transactionId)
        .setUnitId(unitId)
      clientHandler ! Write(adu.toByteString)
    }
  }

  override def postStop(): Unit = {
    queryTimeoutTimer.cancel()
  }

  override def receive: Receive = {
    case idToMessageTemplate: Map[Int, Template] =>
      context.become(waitingForReplies(Map.empty, idToMessageTemplate.keySet, idToMessageTemplate))
  }

  def waitingForReplies(dataSoFar: Map[String, Double],
                        pendingTransactions: Set[Int],
                        idToMessageTemplate: Map[Int, Template]): Receive = {

    case data: ByteString =>
      log.info(s"waiting for $pendingTransactions")
      log.info(s"recieved ByteString: [$data]")
      val adu = ADU.decode(data)
      log.info(s"ByteString decoded to: " +
        s"transactionId: [${adu.mbap.transactionId}]," +
        s"ADU length: [${adu.mbap.length}], " +
        s"unitId: [${adu.mbap.unitId}], " +
        s"PDU length: [${adu.pdu.length}], " +
        s"payload: [${adu.pdu.payload}]")

      val incomingValueMap = idToMessageTemplate(adu.mbap.transactionId).decode(adu.pdu.payload)
      receivedResponse(adu.mbap.transactionId, incomingValueMap, pendingTransactions, dataSoFar, idToMessageTemplate)

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
                       dataSoFar: Map[String, Double],
                       idToMessageTemplate: Map[Int, Template]): Unit = {

    val newPendingTransactions = pendingTransactions - transactionId

    val newDataSoFar = dataSoFar ++ incomingValueMap
    if (newPendingTransactions.isEmpty) {
      context.unwatch(clientHandler)
      requester ! PollResponse(requestId, newDataSoFar) // get rid of this dependency.
      context.stop(self)
    } else {
      context.become(waitingForReplies(newDataSoFar, newPendingTransactions, idToMessageTemplate))
    }
  }
}

