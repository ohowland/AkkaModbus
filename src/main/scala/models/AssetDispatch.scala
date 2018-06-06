package models

abstract class AssetDispatch extends AssetAttrib {
  var power_controller: PowerController = ???
  var cost_of_energy: DispatchModule = ???
}
