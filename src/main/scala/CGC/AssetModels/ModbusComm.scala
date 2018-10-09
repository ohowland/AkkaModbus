package CGC.AssetModels

import akka.actor.{ Actor, ActorLogging, Props }

import java.net.InetAddress
import net.wimpi.modbus.io.ModbusTCPTransaction
import net.wimpi.modbus.msg.{ ModbusRequest, ReadMultipleRegistersRequest, ReadMultipleRegistersResponse }
import net.wimpi.modbus.net.TCPMasterConnection


object ModbusComm {
  def props(hostName: String, port: Int, id: Int, timeout: Int): Props =
    Props(new ModbusComm(hostName, port, id, timeout))

  trait State
  case object Disconnected extends State
  case class Connected(connection: TCPMasterConnection) extends State

  case object Connect
  case object Disconnect
  case object PollSlave
}

class ModbusComm(hostName: String, port: Int, id: Int, timeout: Int) extends Actor with ActorLogging {

  import ModbusComm._

  override def preStart(): Unit = log.info("Modbus Communication Actor started")

  override def postStop(): Unit = log.info("Modbus Communication Actor stopped")

  def receive: Receive = {
    val host = InetAddress.getByName(hostName)
    val connection = new TCPMasterConnection(host)

    connection.setPort(port)
    connection.setTimeout(timeout)
    connection.connect()

    connected(connection)
  }

  def connected(connection: TCPMasterConnection): Receive = {
    case PollSlave =>
      log.info("Sure.. polling...")
      val startingRegister: Int = 0
      val numberOfRegisters: Int = 2

      val request = new ReadMultipleRegistersRequest(startingRegister, numberOfRegisters)
      request.setUnitID(id)

      val transaction = new ModbusTCPTransaction(connection)
      transaction.setRequest(request.asInstanceOf[ModbusRequest]) // What does this do?

      val values = try {
        transaction.execute()
        val response = transaction.getResponse.asInstanceOf[ReadMultipleRegistersResponse]
        response.getRegisters map (_.getValue)
      }


    case Disconnect =>
      connection.close()
      context.become(disconnected(connection))
  }

  def disconnected(connection: TCPMasterConnection): Receive = {
    case PollSlave =>
      connection.connect()
      context.self ! Connect(connection)
      context.self ! PollSlave

  }
}
