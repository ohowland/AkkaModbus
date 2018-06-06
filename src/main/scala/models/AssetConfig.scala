package models

abstract class AssetConfig(val name: String,
                           val rated_kw: Float,
                           val rated_kva: Float) extends AssetAttrib {

}
