import play.api.mvc._
import com.github.tminglei.bind.simple._

object FeatureController extends Controller with MyFormBindSupport {

  def find() = Action { implicit request =>
    val mappings = tmapping(
      "cond" -> text()
    )
    binder.bind(mappings, params).fold(
      errors => status(400, errors),
      { case (cond) =>
        ok(repos.features.find(cond))
      }
    )
  }
}
