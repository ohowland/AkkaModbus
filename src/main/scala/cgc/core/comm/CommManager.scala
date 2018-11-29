package cgc.core.comm

import akka.actor.{Actor, ActorLogging, Props}

import scala.concurrent.duration._
import com.typesafe.config.Config
import modbus.poll.Poll.PollResponse
import modbus.{io, poll}

/** Loads in the Modbus Map and generates the required modbus messsages, loads the configuration for the Client Handler,
  * which includes all information needed by the Client and IO class. Then creates recurring Poll Actors directed at
  * the Client Handler. Poll actors return a value map, which is used by the Spec Device class to update its status.
  */
object CommManager {
  def props(config: Config): Props = Props(new CommManager(config))

  case object PollDevices
  case class UpdateStatus(data: Map[String, Double])
  case class WriteControl(data: Map[String, Double])
}

class CommManager(config: Config) extends Actor with ActorLogging{

  import CommManager._
  import modbus.io._
  import context.dispatcher

  // these values are drawn from the config file for each specific device.
  // want I want, is for the user to be able to register "groups" it wishes to poll information for.
  // the groups correspond to the groups in the modbus map, like status, faults, control, etc.
  // TODO: I think we need to be explicit about what groups are status and what are control.

  private val modbusMap = config.getConfig("communication.map")
  private val templates = ???

  private val unitId = config.getInt("communication.unitId")
  private val pollInterval = config.getDouble("communication.pollInterval").seconds
  private val pollTimeout = config.getDouble("communication.pollTimeout").seconds

  private val clientHandler = context.actorOf(Handler.props(configHandler))

  context.system.scheduler.schedule(3.seconds, 1.seconds, self, PollDevices)

  // spawn client handler + client

  override def preStart(): Unit = { log.info("CommManager started") }
  override def postStop(): Unit = { log.info("CommManager stopped") }

  def receive: Receive = {

    case PollDevices =>
      val requestId = util.Random.nextInt()
      context.actorOf(modbus.poll.Poll.props(
        requestId,
        clientHandler,
        templates,
        unitId,
        requester = self,
        pollTimeout))

    case PollResponse(requestId, dataMap) =>
      dataMap.foreach(println)
  }

  def configHandler: Handler.HandlerConfig = {
    Handler.HandlerConfig(
      config.getString("communication.address"),
      config.getString("communication.name"),
      20)
  }
}
