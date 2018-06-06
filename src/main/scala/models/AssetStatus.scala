package models

/**
  * Base class for
  */
abstract class AssetStatus extends AssetAttrib {
  var kw: Float = 0
  var kvar: Float = 0

  var online: Boolean = false
  var dispatchable: Boolean = false
}
