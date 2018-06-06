/** Title: Composite Grid Controller (CGC)
  * Author: Owen Edgerton
  * Date: 6/5/18
  * Notes:
  */

package models

trait Asset {
  val status: AssetStatus     // Defines object status
  val control: AssetControl   // Defines object control
  val config: AssetConfig     // Defines object configuration
  val comm: AssetComm         // Defines communication interface
  val dispatch: AssetDispatch // Defines object or group behavior

  def addAsset(asset: Asset)
  def removeAsset(asset: Asset)
}

abstract class AssetComposite extends Asset {

  private var mAssets = List[Asset]()

  def addAsset(asset: Asset): Unit = {
    mAssets = asset :: mAssets
  }

  def removeAsset(asset: Asset): Unit = {
    mAssets = mAssets.filterNot(_ == asset)
  }
}