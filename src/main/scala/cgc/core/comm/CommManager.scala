package cgc.core.comm

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.concurrent.duration._
import com.typesafe.config.Config
import modbus.poll.Poll.PollResponse
import modbus.templates

/** Loads in the Modbus Map and generates the required modbus messsages, loads the configuration for the Client Handler,
  * which includes all information needed by the Client and IO class. Then creates recurring Poll Actors directed at
  * the Client Handler. Poll actors return a value map, which is used by the Spec Device class to update its status.
  */
object CommManager {
  def props(config: Config, requestingActor: ActorRef): Props = Props(new CommManager(config, requestingActor))

  case object PollDevices
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
  private val modbusMap = templates.ConfigReader.readResource(deviceName + "ModbusMap.csv")
  private val templateList = templates.Factory
    .createReadHoldingRegistersTemplates(modbusMap, "status", "big").toSet

  // schedule the reoccurring poll actor
  private val pollInterval = config.getDouble("communication.pollInterval").seconds
  context.system.scheduler.schedule(
    initialDelay = 1.seconds,
    interval = pollInterval,
    receiver = self,
    message = PollDevices)

  private val unitId = config.getInt("communication.unitId")
  private val pollTimeout = config.getDouble("communication.pollTimeout").seconds

  override def preStart(): Unit = { log.info("CommManager started") }
  override def postStop(): Unit = { log.info("CommManager stopped") }

  def receive: Receive = {

    case PollDevices =>
      val requestId = util.Random.nextInt()
      context.actorOf(modbus.poll.Poll.props(
        requestId,
        clientHandler,
        templateList,
        unitId,
        requester = self,
        pollTimeout))

    case PollResponse(requestId, dataMap) =>
      dataMap.foreach(println)
      requestingActor ! UpdateStatus(dataMap)

    case WriteControl(data) => ???
  }

  def configHandler: Handler.HandlerConfig = {
    Handler.HandlerConfig(
      config.getString("communication.address"),
      config.getString("name"),
      20)
  }
}