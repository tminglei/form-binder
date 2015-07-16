import org.scalatra.ScalatraServlet
import com.github.tminglei.bind.simple._

class SampleServlet extends ScalatraServlet with MyFormBindSupport {

  get("/:id") {
    val mappings = tmapping(
      "id" -> long()
    )
    binder.bind(mappings, params).fold(
      errors => holt(400, errors),
      { case (id) =>
        Ok(toJson(repos.features.get(id)))
      }
    )
  }
}
