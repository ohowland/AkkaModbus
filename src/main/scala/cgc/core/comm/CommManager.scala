package cgc.core.comm

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}

import scala.concurrent.duration._
import com.typesafe.config.Config
import modbus.io.Handler.{CloseConnection, HandlerConfig}
import modbus.poll.Poll.PollResponse
import modbus.template

/** Loads in the Modbus Map and generates the required modbus messsages, loads the configuration for the Client Handler,
  * which includes all information needed by the Client and IO class. Then creates recurring Poll Actors directed at
  * the Client Handler. Poll actors return a value map, which is used by the Spec Device class to update its status.
  */
object CommManager {
  def props(config: Config, requestingActor: ActorRef): Props = Props(new CommManager(config, requestingActor))

  case object ReadPoll
  case object WritePoll
  case object CloseConnections
  case class UpdateStatus(data: Map[String, Double])
  case class WriteControl(data: Map[String, Double])
}

class CommManager(config: Config, requestingActor: ActorRef) extends Actor with ActorLogging{

  import CommManager._
  import modbus.io._
  import context.dispatcher

  // these values are drawn from the config file for each specific device.
  // want I want, is for the user to be able to register "groups" it wishes to poll information for.
  // the groups correspond to the groups in the modbus map, like status, faults, control, etc.
  // TODO: I think we need to be explicit about what groups are status and what are control.

  // spawn client handler
  private val clientHandler = context.actorOf(Handler.props(configHandler))

  // Create the templates required for this class.
  private val deviceName = config.getString("name")
  private val modbusMap = template.ConfigReader.readResource(deviceName + "ModbusMap.csv")
  private val readStatusTemplates = template.Factory
    .getReadMultipleHoldingRegistersTemplates(modbusMap, "status", "big").toSet
  private val writeControlTemplates = template.Factory
    .getWriteSingleHoldingRegisterTemplates(modbusMap, "control", "big").toSet

  // schedule the recurring read status poll actor
  private val pollInterval = config.getDouble("communication.pollInterval").seconds
  context.system.scheduler.schedule(
    initialDelay = 1.seconds,
    interval = pollInterval,
    receiver = self,
    message = ReadPoll)

  private val unitId = config.getInt("communication.unitId")
  private val pollTimeout = config.getDouble("communication.pollTimeout").seconds

  override def preStart(): Unit = { log.info("CommManager started") }
  override def postStop(): Unit = { log.info("CommManager stopped") }

  def receive: Receive = {

    case ReadPoll =>
      val requestId = util.Random.nextInt()
      context.actorOf(modbus.poll.Poll.props(
        requestId,
        clientHandler,
        readStatusTemplates,
        unitId,
        requester = self,
        pollTimeout))

    case PollResponse(requestId, dataMap) =>
      dataMap.foreach(println)
      requestingActor ! UpdateStatus(dataMap)

    case WriteControl(data) => {

      val requestId = util.Random.nextInt()
      context.actorOf(modbus.poll.Poll.props(
        requestId,
        clientHandler,
        writeControlTemplates,
        unitId,
        requester = self,
        pollTimeout))

    }

    case CloseConnections =>
      clientHandler ! CloseConnection
  }

  def configHandler: HandlerConfig = {
    HandlerConfig(
      config.getString("communication.address"),
      config.getInt("communication.port"),
      config.getString("name"),
      20)
  }
}