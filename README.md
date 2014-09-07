form-binder
===========

`Scalatra`'s built-in data binding and validating framework is a little verbose to use, not handy as `Play!`'s. So I created `form-binder`, a general form data binding and validating framework inspired by `play-data`.

`form-binder` was initially created for my scalatra-based project, but that doesn't mean you can't use it in other scenarios, because it didn't depend on any scalatra specific codes, and very easy to integrate with other frameworks.


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

`form-binder` hasn't built-in support for `scalatra`, though it was created for a scalatra-based project. But you can  do it easily like below:
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


Principle
----------
TODO

Details
----------
TODO

Install
----------
TODO

Build
----------
TODO

Change Log
----------
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
