package CGC.Modbus

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import java.net.InetAddress
import scala.util.control.NonFatal

import net.wimpi.modbus.io.ModbusTCPTransaction
import net.wimpi.modbus.msg.{ReadMultipleRegistersRequest, ReadMultipleRegistersResponse}
import net.wimpi.modbus.net.TCPMasterConnection

import scala.concurrent.duration._

object Comm {
  def props(config: ModbusConfig): Props = Props(new Comm(config))


  case class ModbusConfig(hostName: String, port: Int, id: Int, timeout: Int)

  sealed trait ModbusMessage
  case class ReqReadHoldingRegisters(requestId: Long, startAddress: Int, numberOfRegisters: Int ) extends ModbusMessage
  case class RespReadHoldingRegisters(requestId: Long, response: Option[List[Int]]=None) extends ModbusMessage
}

class Comm(config: Comm.ModbusConfig) extends Actor with ActorLogging {

  import Comm._

  override def preStart(): Unit =
    log.info("Modbus Communication Actor started for target host: {}", config.hostName)

  override def postStop(): Unit =
    log.info("Modbus Communication Actor stopped for target host: {}", config.hostName)

  def getConnection(hostName: String, port: Int, timeout: Int): Future[TCPMasterConnection] = Future {
    val connection = new TCPMasterConnection(InetAddress.getByName(hostName))
    connection.setPort(port)
    connection.setTimeout(timeout)
    connection.connect()
    connection
  }

  /** readHoldingRegisters returns a Future to contain the Modbus poll response values defined by startAddress
    * and numberOfRegisters
    *
    * @param connection TCPMasterConnection
    * @param startAddress
    * @param numberOfRegisters
    * @return List[Int]
    */
  def getHoldingRegisters(connection: TCPMasterConnection,
                          startAddress: Int,
                          numberOfRegisters: Int): Future[Option[List[Int]]] = {

      val request = new ReadMultipleRegistersRequest(startAddress, numberOfRegisters)
      request.setUnitID(config.id)

      val transaction = new ModbusTCPTransaction(connection)
      transaction.setReconnecting(false) // Close the connection after request
      transaction.setRequest(request)

      val response: Future[List[Int]] = Future {
        transaction.execute()
        transaction.getResponse
      }.map { resp => resp.asInstanceOf[ReadMultipleRegistersResponse].getRegisters.map { reg => reg.getValue }.toList }
      response.map(Some(_))
  }

  def receive: Receive = {
    /**
      * Returns a RespHoldingRegisters message composed of the holding registers specifed by startAddress
      * to numberOfRegisters to sender.
      *
      * Notes:
      * Future.map returns a Future[T] where T is the value returned by map. This is how chained futures are created.
      * Future.foreach always returns Unit. This seems to be how you end a chain of futures..
      */
    case ReqReadHoldingRegisters(requestId: Long, startAddress: Int, numberOfRegisters: Int) =>

      val response: Future[Option[List[Int]]] =
        getConnection(config.hostName, config.port, config.timeout)
          .flatMap { getHoldingRegisters( _, startAddress, numberOfRegisters) }
          .recover { case NonFatal(e) => None }

      pipe(convertToRespReadHoldingRegisters(response)) to sender()

      def convertToRespReadHoldingRegisters(response: Future[Option[List[Int]]]): Future[RespReadHoldingRegisters] = {
        response.map( RespReadHoldingRegisters(requestId, _) )
      }



  }
}