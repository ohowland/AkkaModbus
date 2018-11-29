import com.typesafe.config.ConfigFactory
import java.io.File

val testres = getClass.getResource("/application.conf")
val config = ConfigFactory.parseFile(new File(testres.getPath)).resolve

config.getInt("SEL547.version")
config.getString("SEL547.description")
