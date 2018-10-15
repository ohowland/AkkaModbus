
// Load a Modbus Map
import CGC.AssetCommunications.ModbusCSVReader
val fileURL = getClass.getResource("/testModbusMap.csv")
val modbusMap = ModbusCSVReader.readCSVToRegisterList(fileURL)


// Create templates for the ReqReadHoldingRegister mesage
import CGC.AssetCommunications.ModbusMessageFactory
val reqTemplateMessages =
  ModbusMessageFactory.createReadHoldingRegisterMessageTemplates(modbusMap, "status")

// Create a map of the ReqReadHoldingRegisterTemplate to a RespReadHoldingRegisterTemplate
import CGC.AssetCommunications.ModbusDecodeMessages
val respTemplateMessageMap: Map[ModbusMessageFactory.ReadHoldingRegistersTemplate, ModbusDecodeMessages.DecodeRespReadHoldingRegisterTemplate] = (for {
    reqMessage <- reqTemplateMessages
    respMessage = ModbusDecodeMessages.CreateRespHoldingRegisterMessageTemplate(reqMessage, modbusMap, "big")
  } yield reqMessage -> respMessage).toMap

// Generate fake ReqReadHoldingRegister response data, List[Short]
val testResponseValueList: List[Short] = List(12, 1, 0)

// Get the RespReadHoldingRegister template for the ReqReadHoldingRegisterTemplate
val respTemplate = respTemplateMessageMap.get(reqTemplateMessages.head)

// Decode values
val responseValueMap: Map[String, Double] = respTemplate.get.decode(testResponseValueList)