package models

abstract class AssetComm(val address: String,
                         val port: Int,
                         val id: Int) extends AssetAttrib {

  def connect()
  def disconnect()

}
