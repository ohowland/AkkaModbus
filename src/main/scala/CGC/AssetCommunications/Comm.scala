package CGC.AssetCommunications

import java.net.InetAddress

import CGC.AssetCommunications.Comm.ModbusMessage
import akka.actor.{Actor, ActorLogging, Props}
import net.wimpi.modbus.io.ModbusTCPTransaction
import net.wimpi.modbus.msg.{ModbusRequest, ReadMultipleRegistersRequest, ReadMultipleRegistersResponse}
import net.wimpi.modbus.net.TCPMasterConnection

import scala.concurrent.duration._

object Comm {
  def props(config: ModbusConfig): Props =
    Props(new Comm(config))

  case class ModbusConfig(hostName: String,
                          port: Int,
                          id: Int,
                          timeout: Int,
                          maxRetryAttempts: Int)

  case class ModbusRegsiter(name: String,
                            address: Int,
                            datatype: ModbusDatatype,
                            access: ModbusAccessType,
                            group: String,
                            block: Int)

  sealed trait ModbusDatatype
  case object U16 extends ModbusDatatype
  case object U32 extends ModbusDatatype
  case object I16 extends ModbusDatatype
  case object I32 extends ModbusDatatype
  case object F32 extends ModbusDatatype
  case object F64 extends ModbusDatatype

  sealed trait ModbusAccessType
  case object Read      extends ModbusAccessType
  case object Write     extends ModbusAccessType
  case object ReadWrite extends ModbusAccessType

  sealed trait ModbusMessage
  case class ReqReadHoldingRegisters(requestId: Long, startAddress: Int, numberOfRegisters: Int ) extends ModbusMessage
  case class RespReadHoldingRegisters(requestId: Long, response: Option[List[Int]])               extends ModbusMessage

  case class ReqWriteHoldingRegister(requestId: Long, address: Int, value: Int) extends ModbusMessage
  case class RespWriteHoldingRegister(requestId: Long)                          extends ModbusMessage

  case class ConnectionTimedOut(requestId: Long) extends ModbusMessage
  case object CompleteTimeout                    extends ModbusMessage
}

class Comm(config: Comm.ModbusConfig) extends Actor with ActorLogging {

  val system = akka.actor.ActorSystem("ModbusComm")
  import Comm._
  import system.dispatcher


  override def preStart(): Unit =
    log.info("Modbus Communication Actor started for target host: {}", config.hostName)

  override def postStop(): Unit =
    log.info("Modbus Communication Actor stopped for target host: {}", config.hostName)

  def getConnection(hostName: String, port: Int, timeout: Int): Option[TCPMasterConnection] = {
    try {
      val host = InetAddress.getByName(hostName)
      val connection = new TCPMasterConnection(host)
      connection.setPort(port)
      connection.setTimeout(timeout)
      connection.connect()
      Some(connection)
    } catch {
      case e: Exception => None
    }
  }

  /** transactMessages attempts to poll the TCPMasterConnection with the registers in the list
    *
    * @param connection TCPMasterConnection
    * @param startAddress
    * @param numberOfRegisters
    * @return Option[List[Int]] this needs to be changed to a typed parameter
    */
  def readHoldingRegisters(connection: TCPMasterConnection, startAddress: Int, numberOfRegisters: Int): Option[List[Int]] = {
    log.info("Polling: {}", config.hostName)

    val request = new ReadMultipleRegistersRequest(startAddress, numberOfRegisters)
    request.setUnitID(config.id)

    val transaction = new ModbusTCPTransaction(connection)
    transaction.setRequest(request.asInstanceOf[ModbusRequest]) // What does this do?

    try {
      transaction.execute()
      val values = transaction.getResponse.asInstanceOf[ReadMultipleRegistersResponse]
      log.info("{} raw response: " + values.getRegisters.map(_.getValue).toList.toString)
      Some(values.getRegisters.map(_.getValue).toList)
    } catch {
      case e: Exception => None
    }
  }

  def connectionManager(config: ModbusConfig, retryAttempt: Int): Unit = {
    log.info("Querying connection manager")
    if (retryAttempt < config.maxRetryAttempts) {
      getConnection(config.hostName, config.port, config.timeout) match {
        case Some(c) => context.become(idle(Some(c)))
        case None => connectionManager(config, retryAttempt + 1)
      }
    } else {
      log.info("Maximum connection retry attempts exceeded, entering timeout state")
      context.become(communicationFailed)
      system.scheduler.scheduleOnce(3.seconds, context.self, CompleteTimeout)
      // schedule a message that is return
    }
  }

  /** Actor Reveive Message Functions */
  def receive: Receive = {
    idle(None)
  }

  /** Actor timeout state. Actor will not attempt to transact messages
    * until the timeout state has passed.
    * @return
    */
  def communicationFailed: Receive = {
    case CompleteTimeout => context.become(idle(None))

    case ReqReadHoldingRegisters(id, _, _) => sender() ! ConnectionTimedOut(id)
  }

  /** Standard operating state for the Communications Actor
    * will reviece poll requests and attempt to transact the message
    * with the target device
    * @param connection The TCP Master Connection object wrapped in an Option
    * @return
    */
  def idle(connection: Option[TCPMasterConnection]): Receive = {
    /** Poll the slave for information and return Option[Array[Int]] */
    case ReqReadHoldingRegisters(id, startAddress, numberOfRegisters) =>
      if (connection match {
        case Some(c) => c.isConnected
        case None => false
      }) {
        // valid connection: attempt to transact a message
        val response = readHoldingRegisters(connection.get, startAddress, numberOfRegisters)
        sender() ! RespReadHoldingRegisters(id, response)
      } else {
        // invalid connection: request reconnect through the connection manager
        connectionManager(config, 0)
        context.self forward ReqReadHoldingRegisters(id, startAddress, numberOfRegisters)
      }
  }
}