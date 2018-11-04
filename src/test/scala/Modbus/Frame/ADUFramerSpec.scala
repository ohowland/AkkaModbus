package Modbus.Frame

class ADUFramerSpec {

  import akka.actor.ActorSystem
  import akka.testkit.{TestKit, TestProbe}
  import org.scalatest._

  class ADUFramerSpec(_system: ActorSystem) extends TestKit(_system)
    with Matchers
    with WordSpecLike
    with BeforeAndAfterAll {

    def this() = this(ActorSystem("ADUFramerSpec"))

    "An Application Data Unit Framer" should {
      "convert ADU objects into ByteStreams" in {

        val aduFramerActor = system.actorOf(ADUFramer.props)
        val probe = TestProbe()

        val testPDU: PDU = RequestReadHoldingRegisters(startAddress = 1, numberOfRegisters = 2)
        val testMBAP: MBAP = MBAP(transactionId = 123, length = testPDU.length, unitId = 1)
        val testADU: ADU = ADU(testMBAP, testPDU)
      }
    }
  }


}
