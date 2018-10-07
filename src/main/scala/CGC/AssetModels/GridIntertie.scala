package CGC.AssetModels

import akka.actor.{ Actor, ActorLogging, Props}

/** The Grid Intertie Actor is a Finite State Machine (FSM) used to track the Grid Intertie State.
  * It manages a Communications actor which gathers state information from the physical device.
  * There needs to be some sort of recurring query on the communications actor.
  *
  */
object GridIntertie {
  def props: Props = Props(new GridIntertie)

  // Grid Intertie Messages
  sealed trait GridIntertieMessage

  // Command Msgs
  final case class ReqGridConnected(requestId: Long)                extends GridIntertieMessage
  final case class ReqGridDisconnected(requestId: Long)             extends GridIntertieMessage
  final case class RespGridStateCommand(requestId: Long)            extends GridIntertieMessage

  // Status Getter Msg
  final case class ReqGetStatus(requestId: Long)                    extends GridIntertieMessage
  final case class RespGetStatus(requestId: Long, status: Status)   extends GridIntertieMessage

  // Status Setter Msg
  final case class ReqSetStatus(requestId: Long, newStatus: Status) extends GridIntertieMessage
  final case class RespSetStatus(requestId: Long)                   extends GridIntertieMessage

  // State Request Msg
  final case class ReqState(requestId: Long)                        extends GridIntertieMessage
  final case class RespState(requestId: Long, state: GridState)     extends GridIntertieMessage

  // Grid Intertie States
  sealed trait GridState
  final case object GridDisconnected        extends GridState
  final case object GridConnectRequested    extends GridState
  final case object GridConnected           extends GridState
  final case object GridDisconnectRequested extends GridState

  // Grid Intertie Private Data
  case class Status(online: Boolean, kW: Double, kVar: Double)
}

class GridIntertie extends Actor with ActorLogging {

  import GridIntertie._

  //case class Dispatch(realPowerExportLimit: Double, realPowerImportLimit: Double)

  override def preStart(): Unit = {
    log.info("Grid Intertie Actor started")
    // launch communications actor here. The communications actor updates at some interval. it
    // could push information up to this object through a message. this object could expect that the
    // message comes in a certain period of time.
    // keep a ref to that actor, so we accept update status requests from only that actor.
  }
  override def postStop(): Unit = log.info("Grid Intertie Actor stopped")

  override def receive: Receive = {
    // need a better way to initalize
    gridDisconnected(Status(online = false, kW = 0.0, kVar = 0), GridDisconnected)
  }

  /** gridDisconnected
    * STATE: The grid intertie is open.
    * @param status
    * @return
    */
  def gridDisconnected(status: Status, state: GridState): Receive = {
    case ReqGridConnected(id) =>
      sender() ! RespGridStateCommand(id)
      log.info("Grid Intertie Actor State: GridConnectRequested")
      context.become(gridConnectRequested(status, GridConnectRequested))

    case ReqGridDisconnected(id) =>
      sender() ! RespGridStateCommand(id)

    case ReqSetStatus(id, newStatus) =>
      sender() ! RespSetStatus(id)
      if (newStatus.online) {
        log.info("Grid Intertie Actor State: gridConnected")
        context.become(gridConnected(newStatus, GridConnected))
      }
      else context.become(gridDisconnected(newStatus, GridDisconnected))

    case ReqGetStatus(id) =>
      sender() ! RespGetStatus(id, status)

    case ReqState(id) =>
      sender() ! RespState(id, state)
  }

  /** gridConnectRequested
    * STATE: The grid intertie has been commanded open and is currently waiting for confirmation
    * @param status
    * @return
    */
  def gridConnectRequested(status: Status, state: GridState): Receive = {
    case ReqSetStatus(id, newStatus) =>
      sender() ! RespSetStatus(id)
      if (newStatus.online) {
        log.info("Grid Intertie Actor State: GridConnected")
        context.become(gridConnected(newStatus, GridConnected))
      }
      else context.become(gridConnectRequested(newStatus, GridConnectRequested))

    case ReqGetStatus(id) =>
      sender() ! RespGetStatus(id, status)

    case ReqState(id) =>
      sender() ! RespState(id, state)
  }

  /** gridConnected
    * STATE: The grid intertie is closed.
    * @param status
    * @return
    */
  def gridConnected(status: Status, state: GridState): Receive = {

    case ReqGridDisconnected(id) =>
      sender() ! RespGridStateCommand(id)
      log.info("Grid Intertie Actor State: GridConnectDisconnectRequested")
      context.become(gridDisconnectRequested(status, GridDisconnectRequested))

    case ReqGridConnected(id) =>
      sender() ! RespGridStateCommand(id)

    case ReqSetStatus(id, newStatus) =>
      sender() ! RespSetStatus(id)
      if (!newStatus.online) {
        log.info("Grid Intertie Actor State: GridDisconnected")
        context.become(gridDisconnected(newStatus, GridDisconnected))
      }
      else context.become(gridConnected(newStatus, GridConnected))

    case ReqGetStatus(id) =>
      sender() ! RespGetStatus(id, status)

    case ReqState(id) =>
      sender() ! RespState(id, state)
  }

  /** gridDisconnectRequested
    * STATE: The grid intertie has been commanded open, and is currently waiting for confirmation that
    * the breaker has opened.
    * @param status
    * @return
    */
  def gridDisconnectRequested(status: Status, state: GridState): Receive = {
    case ReqSetStatus(id, newStatus) =>
      sender() ! RespSetStatus(id)
      if (!newStatus.online) {
        log.info("Grid Intertie Actor State: GridDisconnected")
        context.become(gridDisconnected(newStatus, GridDisconnected))
      }
      else context.become(gridDisconnectRequested(newStatus, GridDisconnectRequested))

    case ReqGetStatus(id) =>
      sender() ! RespGetStatus(id, status)

    case ReqState(id) =>
      sender() ! RespState(id, state)
  }
}
