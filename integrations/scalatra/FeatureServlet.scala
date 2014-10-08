import org.scalatra.ScalatraServlet
import com.github.tminglei.bind.simple._

class FeatureServlet extends ScalatraServlet with MyFormBindSupport {

  get("/:id") {
    val mappings = tmapping(
      "id" -> long()
    )
    binder.bind(mappings, params) { case (id) =>
      repos.features.get(id)
    }
  }
}
