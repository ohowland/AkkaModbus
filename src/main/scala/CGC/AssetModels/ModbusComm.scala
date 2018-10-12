package CGC.AssetModels

import akka.actor.{ Actor, ActorLogging, Props }
import scala.concurrent.duration._

import java.net.InetAddress
import net.wimpi.modbus.io.ModbusTCPTransaction
import net.wimpi.modbus.msg.{ ModbusRequest, ReadMultipleRegistersRequest, ReadMultipleRegistersResponse }
import net.wimpi.modbus.net.TCPMasterConnection

object ModbusComm {
  def props(config: ModbusConfig): Props =
    Props(new ModbusComm(config))

  case class ModbusRegsiter(name: String,
                            address: Int,
                            datatype: ModbusDataType,
                            access: ModbusAccessType,
                            group: Int,
                            block: Int)

  case class ModbusConfig(hostName: String,
                          port: Int,
                          id: Int,
                          timeout: Int,
                          maxRetryAttempts: Int,
                          registers: Array[ModbusRegsiter])

  sealed trait ModbusDataType
  case object U16 extends ModbusDataType
  case object U32 extends ModbusDataType
  case object I16 extends ModbusDataType
  case object I32 extends ModbusDataType
  case object F32 extends ModbusDataType
  case object F64 extends ModbusDataType

  sealed trait ModbusAccessType
  case object Read extends ModbusAccessType
  case object Write extends ModbusAccessType
  case object ReadWrite extends ModbusAccessType

  sealed trait ModbusMessage
  case class reqPollSlave(requestId: Long) extends ModbusMessage
  case class respPollSlave(requestId: Long, response: Option[Array[Int]]) extends ModbusMessage
  case object completeTimeout extends ModbusMessage
}

class ModbusComm(config: ModbusComm.ModbusConfig) extends Actor with ActorLogging {

  val system = akka.actor.ActorSystem("ModbusComm")
  import system.dispatcher
  import ModbusComm._


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

  def transactMessage(connection: TCPMasterConnection, registers: Array[ModbusRegsiter]): Option[Array[Int]] = {
    log.info("Polling: {}", config.hostName)
    val startingRegister: Int = 0
    val numberOfRegisters: Int = 2

    val request = new ReadMultipleRegistersRequest(startingRegister, numberOfRegisters)
    request.setUnitID(config.id)

    val transaction = new ModbusTCPTransaction(connection)
    transaction.setRequest(request.asInstanceOf[ModbusRequest]) // What does this do?

    try {
      transaction.execute()
      val values = transaction.getResponse.asInstanceOf[ReadMultipleRegistersResponse]
      Some(values.getRegisters map (_.getValue))
    } catch {
      case e: Exception => None
    }
  }

  def connectionManager(config: ModbusConfig, retryAttempt: Int): Unit = {
    if (retryAttempt <= config.maxRetryAttempts) {
      getConnection(config.hostName, config.port, config.timeout) match {
        case Some(c) => context.become(idle(Some(c)))
        case None => connectionManager(config, retryAttempt + 1)
      }
    } else {
      context.become(communicationFailed)
      system.scheduler.scheduleOnce(3.seconds, context.self, completeTimeout)
      // schedule a message that is return
    }
    context.self forward reqPollSlave
  }


  /** Actor Reveive Message Functions */
  def receive: Receive = {
    idle(None)
  }

  def communicationFailed: Receive = {
    case completeTimeout => context.become(idle(None))
  }

  def idle(connection: Option[TCPMasterConnection]): Receive = {

    /** Poll the slave for information and return Option[Array[Int]] */
    case reqPollSlave(id) =>

      if ( connection match {
        case Some(c) => c.isConnected
        case None => false
      }) transactMessage(connection.get, config.registers)
      else { connectionManager(config, 0) }
  }
}
