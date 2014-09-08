form-binder
===========

`form-binder` is a general form data binding and validating framework inspired by `play-data`.

`form-binder` was initially created for my scalatra-based project, but you can use it in other scenarios, since it didn't depend on any scalatra codes, and easy to integrate with other frameworks.


Usage
------
A **raw** usage case (aka. not integrated with other framework) is as below:
```scala
import com.github.tminglei.bind.simple._

case class Order(name: String, email: String, price: Float, count: Int)

val binder = expandJsonData("json") pipe_: FormBinder(messages).withErr(errsToJson4s)
val mappings = tmapping(
  "id" -> long(),
  "json" -> mapping(
    "name"  -> text(maxlength(100)),
    "email" -> text(email()),
    "price" -> (cleanPrefix("$") pipe_: float()),
    "count" -> number().verifying(min(3))
  )(Order.apply).label("order")
).verifying { case ((id, order), messages) =>
    //do some extra verifying
    Nil
  }

///
val data = Map(
  "id" -> "133",
  "json" -> """{"name":"xxxx", "email":"123@example.com", "price":"$137.5", "count":5}"""
)
binder.bind(mappings, data) { case (id, order) =>
  //do something ...
}
```

`form-binder` **hasn't built-in support for Scalatra**, though it was created for a scalatra-based project. But you can do it youself easily like below:
```scala
// firstly, create your FormBindSupport.scala
import com.github.tminglei.bind.simple._

object MyFormBindSupport {
  val BindMessagesKey = "bind-messages"
}

trait MyFormBindSupport extends I18nSupport { this: ScalatraBase =>
  import MyFormBindSupport._

  before() {
    request(BindMessagesKey) = Messages(locale, bundlePath = "i18n/bind-messages")
  }

  def binder(implicit request: HttpServletRequest) =
    expandJsonData("json") pipe_: FormBinder(bindMessages.apply).withErr(errsToJson4s)

  ///
  private def bindMessages(implicit request: HttpServletRequest): Messages = if (request == null) {
    throw new ScalatraException("There needs to be a request in scope to call bindMessages")
  } else {
    request.get(BindMessagesKey).map(_.asInstanceOf[Messages]).orNull
  }
}

// then use it in your xxxxxxxxServlet.scala
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
```


Principle & Details
-------------------
The core of `form-binder` is a composite mapping
- when fed a data map, top/parent mapping will direct child mappings to start their jobs
- child or child of child mappings will get their string value from the data map, and do the validation or convertion
- their parent mapping will continue: return/report found errors from children; or do more data checking if necessary, then return; or make a case class or tuple, then return to its parent
- finnaly, we got an error sequence if validation errors found, or a full case class or tuple
- if data is valid, our consume codes will be fed an expected object, then do our jobs

_**p.s. `form-binder` will validate data firstly; if errors found, stop and report; if no errors found, then convert to result object.**_

(... to be continued)

Install & Build
-------------------
TODO

Acknowledgements
-----------------
Firstly, I want to thank `Play!` development team. Except them, I also want to say thanks to
- [Naoki Takezoe](https://github.com/takezoe) and his [scalatra-forms](https://github.com/takezoe/scalatra-forms), which provide a simplified/improved `play-data` implementation for scalatra;
- [Cody Allen](https://github.com/ceedubs) and his [scrutinator](https://github.com/ceedubs/scrutinator), which showed to me an elegant `shapeless` and `ScalaCheck` usage, and data binding definition and swagger param declaration merging.

**I got much from your works. Thank you all very much!**

License
---------
The BSD License, Minglei Tu &lt;tmlneu@gmail.com&gt;
