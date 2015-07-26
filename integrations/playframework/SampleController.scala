import play.api.mvc._
import com.github.tminglei.bind.simple._

object SampleController extends Controller with MyFormBindSupport {
  ///--
  import scala.collection.convert.WrapAsScala._
  def parameterMap(request: HttpServletRequest) = request.getParameterMap.map { case (key, values) => (key, values.toSeq) }.toMap
  ///--

  def find() = Action { implicit request =>
    val mappings = tmapping(
      "cond" -> text()
    )
    binder.bind(mappings, data(parameterMap(request))).fold(
      errors => status(400, errors),
      { case (cond) =>
        ok(repos.features.find(cond))
      }
    )
  }
}
