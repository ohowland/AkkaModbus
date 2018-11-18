package CGC.comm

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, Props}
import modbus.io.Handler.HandlerConfig
import modbus.{io, poll}

/** Loads in the Modbus Map and generates the required modbus messsages, loads the configuration for the Client Handler,
  * which includes all information needed by the Client and IO class. Then creates recurring Poll Actors directed at
  * the Client Handler. Poll actors return a value map, which is used by the Spec Device class to update its status.
  */

object CommManager {
  def props(): Props = Props(new CommManager)
}

class CommManager extends Actor with ActorLogging{

  def receive: Receive = ???

}
