package modbus.frame

import modbus.template._
import org.scalatest._

import scala.util.Random

class EncodeFrameSpec() extends Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  "An Application Data Unit Encoder" should {
    "translate a single RequestReadHoldingRegisters containing one register to an ADU frame " in {
      val testRegister1 = Modbus.Register("test1", 1, Modbus.U16, "status", 1, 1)
      val testRegister2 = Modbus.Register("badtest1", 2, Modbus.U16, "control", 2, 1)
      val testRegister3 = Modbus.Register("badtest2", 3, Modbus.U16, "control", 2, 1)

      val testModbusMap = List(testRegister1, testRegister2, testRegister3)
      val groupName = "status"
      val endianness = "big"

      val templates = modbus.template.Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, groupName, endianness)
      val unitId = 1

      val requestMessageAndIdSet = for {
        template <- templates
        transactionId = util.Random.nextInt(65535) & 0xFF
      } yield (modbus.frame.Factory.instanceOf(template)
        .setTransactionId(transactionId)
        .setUnitId(unitId), transactionId)

      for (requestMessageAndId <- requestMessageAndIdSet) {
        val requestMessage = requestMessageAndId._1
        val transactionId = requestMessageAndId._2
        val testPDU = RequestReadMultipleHoldingRegisters(1, 1)
        requestMessage should ===(ADU(MBAP(transactionId, testPDU.length + 1, unitId), testPDU))
      }
    }

    "translate a single RequestReadHoldingRegisters containing multiple registers to an ADU frame " in {
      val testRegister1 = Modbus.Register("test1", 1, Modbus.U16, "status", 1, 1)
      val testRegister2 = Modbus.Register("test2", 2, Modbus.U16, "status", 1, 1)
      val testRegister3 = Modbus.Register("test3", 3, Modbus.U16, "status", 1, 1)

      val testModbusMap = List(testRegister1, testRegister2, testRegister3)
      val groupName = "status"
      val endianness = "big"

      val templates = modbus.template.Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, groupName, endianness)
      val unitId = 1

      val requestMessageAndIdSet = for {
        template <- templates
        transactionId = util.Random.nextInt(65535) & 0xFF
      } yield (modbus.frame.Factory.instanceOf(template)
        .setTransactionId(transactionId)
        .setUnitId(unitId), transactionId)

      for (requestMessageAndId <- requestMessageAndIdSet) {
        val requestMessage = requestMessageAndId._1
        val transactionId = requestMessageAndId._2
        val PDU = RequestReadMultipleHoldingRegisters(1, 3)
        requestMessage should ===(ADU(MBAP(transactionId, PDU.length + 1, unitId), PDU))
      }
    }

    "translate a multiple RequestReadHoldingRegisters containing multiple registers to an ADU frame " in {
      val message1StartRegister = 1
      val message1LastRegister = 2
      val message2StartRegister = 6
      val message2LastRegister = 7

      val testRegister1 = Modbus.Register("test1", message1StartRegister, Modbus.U16, "status", 1, 1)
      val testRegister2 = Modbus.Register("test2", message1LastRegister, Modbus.U16, "status", 1, 1)
      val testRegister3 = Modbus.Register("test3", message2StartRegister, Modbus.U16, "status", 2, 1)
      val testRegister4 = Modbus.Register("test4", message2LastRegister, Modbus.U16, "status", 2, 1)

      val testModbusMap = List(testRegister1, testRegister2, testRegister3, testRegister4)
      val groupName = "status"
      val endianness = "big"

      val templates = modbus.template.Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, groupName, endianness)
      val unitId = 1
      val validateSet = for {
        template <- templates
        transactionId = util.Random.nextInt(65535) & 0xFF
      } yield (modbus.frame.Factory.instanceOf(template)
        .setTransactionId(transactionId)
        .setUnitId(unitId), transactionId)

      val requestMessage1 = validateSet(0)._1
      val transactionId1 = validateSet(0)._2
      val testPDU1 =
        RequestReadMultipleHoldingRegisters(message1StartRegister, message1LastRegister - message1StartRegister + 1)

      requestMessage1 should ===(ADU(MBAP(transactionId1, testPDU1.length + 1, unitId), testPDU1))

      val requestMessage2 = validateSet(1)._1
      val transactionId2 = validateSet(1)._2
      val testPDU2 =
        RequestReadMultipleHoldingRegisters(message2StartRegister, message2LastRegister - message2StartRegister + 1)

      requestMessage2 should ===(ADU(MBAP(transactionId2, testPDU2.length + 1, unitId), testPDU2))
    }

    "translate a single RequestWriteHoldingRegisters to an ADU frame" in {
      //def encode(template: ModbusMessageTemplate, transactionId: Int, unitId: Int): ADU
      //MessageFactory.createWriteHoldingRegistersTemplates()
    }
  }
}

