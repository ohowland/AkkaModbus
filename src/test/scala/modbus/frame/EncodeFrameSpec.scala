package modbus.frame

import modbus.templates.{Factory, ModbusTypes}
import org.scalatest._

import scala.util.Random

class EncodeFrameSpec() extends Matchers
  with WordSpecLike
  with BeforeAndAfterAll {

  "An Application Data Unit Encoder" should {
    "translate a single RequestReadHoldingRegisters containing one register to an ADU frame " in {
      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)
      val testRegister2 = ModbusTypes.ModbusRegister("badtest1", 2, ModbusTypes.U16, "control", 2)
      val testRegister3 = ModbusTypes.ModbusRegister("badtest2", 3, ModbusTypes.U16, "control", 2)

      val testModbusMap = List(testRegister1, testRegister2, testRegister3)
      val groupName = "status"
      val endianness = "big"

      val templates = Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, groupName, endianness)
      val unitId = 1

      val requestMessageAndIdSet = for {
        template <- templates
        transactionId = util.Random.nextInt(65535) & 0xFF
      } yield (EncodeFrame.encode(template, transactionId, unitId), transactionId)

      for (requestMessageAndId <- requestMessageAndIdSet) {
        val requestMessage = requestMessageAndId._1
        val transactionId = requestMessageAndId._2
        val testPDU = RequestReadHoldingRegisters(1, 1)
        requestMessage should ===(ADU(MBAP(transactionId, testPDU.length + 1, unitId), testPDU))
      }
    }

    "translate a single RequestReadHoldingRegisters containing multiple registers to an ADU frame " in {
      val testRegister1 = ModbusTypes.ModbusRegister("test1", 1, ModbusTypes.U16, "status", 1)
      val testRegister2 = ModbusTypes.ModbusRegister("test2", 2, ModbusTypes.U16, "status", 1)
      val testRegister3 = ModbusTypes.ModbusRegister("test3", 3, ModbusTypes.U16, "status", 1)

      val testModbusMap = List(testRegister1, testRegister2, testRegister3)
      val groupName = "status"
      val endianness = "big"

      val templates = Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, groupName, endianness)
      val unitId = 1

      val requestMessageAndIdSet = for {
        template <- templates
        transactionId = util.Random.nextInt(65535) & 0xFF
      } yield (EncodeFrame.encode(template, transactionId, unitId), transactionId)

      for (requestMessageAndId <- requestMessageAndIdSet) {
        val requestMessage = requestMessageAndId._1
        val transactionId = requestMessageAndId._2
        val PDU = RequestReadHoldingRegisters(1, 3)
        requestMessage should ===(ADU(MBAP(transactionId, PDU.length + 1, unitId), PDU))
      }
    }

    "translate a multiple RequestReadHoldingRegisters containing multiple registers to an ADU frame " in {
      val message1StartRegister = 1
      val message1LastRegister = 2
      val message2StartRegister = 6
      val message2LastRegister = 7

      val testRegister1 = ModbusTypes.ModbusRegister("test1", message1StartRegister, ModbusTypes.U16, "status", 1)
      val testRegister2 = ModbusTypes.ModbusRegister("test2", message1LastRegister, ModbusTypes.U16, "status", 1)
      val testRegister3 = ModbusTypes.ModbusRegister("test3", message2StartRegister, ModbusTypes.U16, "status", 2)
      val testRegister4 = ModbusTypes.ModbusRegister("test4", message2LastRegister, ModbusTypes.U16, "status", 2)

      val testModbusMap = List(testRegister1, testRegister2, testRegister3, testRegister4)
      val groupName = "status"
      val endianness = "big"

      val templates = Factory.getReadMultipleHoldingRegistersTemplates(testModbusMap, groupName, endianness)
      val unitId = 1
      val validateSet = for {
        template <- templates
        transactionId = util.Random.nextInt(65535) & 0xFF
      } yield (EncodeFrame.encode(template, transactionId, unitId), transactionId)

      val requestMessage1 = validateSet(0)._1
      val transactionId1 = validateSet(0)._2
      val testPDU1 =
        RequestReadHoldingRegisters(message1StartRegister, message1LastRegister - message1StartRegister + 1)

      requestMessage1 should ===(ADU(MBAP(transactionId1, testPDU1.length + 1, unitId), testPDU1))

      val requestMessage2 = validateSet(1)._1
      val transactionId2 = validateSet(1)._2
      val testPDU2 =
        RequestReadHoldingRegisters(message2StartRegister, message2LastRegister - message2StartRegister + 1)

      requestMessage2 should ===(ADU(MBAP(transactionId2, testPDU2.length + 1, unitId), testPDU2))
    }

    "translate a single RequestWriteHoldingRegisters to an ADU frame" in {
      //def encode(template: ModbusMessageTemplate, transactionId: Int, unitId: Int): ADU
      //MessageFactory.createWriteHoldingRegistersTemplates()
    }
  }
}

