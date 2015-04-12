form-binder
===========
[![Build Status](https://travis-ci.org/tminglei/form-binder.svg?branch=master)](https://travis-ci.org/tminglei/form-binder)


Form-binder is a micro data binding and validating framework, easy to use and hack.

> _It was initially created for my [`Scalatra`](https://github.com/scalatra/scalatra)-based project, but it's for general purpose. You can easily integrate and use it with other frameworks._


Features
-------------
- very lightweight, only ~900 lines codes (framework + built-in extensions)
- easy use, no verbose codes, and what you see is what you get
- high customizable, you can extend almost every executing point
- easily extensible, every extension interface is an alias of `FunctionN`
- immutable, you can share mapping definition object safely


Usage
-------------
![form-binder description](https://github.com/tminglei/form-binder/raw/master/form-binder-desc.png)

1. define your binder
2. define your mappings
3. prepare your data
4. bind and consume

> _p.s. every points above (1)/(2)/(3)/(4)/(5) are extendable and you can easily customize it, more dev and usage details pls check [source codes](https://github.com/tminglei/form-binder/tree/master/src/main/scala/com/github/tminglei/bind) and [test cases](https://github.com/tminglei/form-binder/tree/master/src/test/scala/com/github/tminglei/bind)._


Install & Integrate
--------------------
To use `form-binder`, pls add the dependency to your [sbt](http://www.scala-sbt.org/ "slick-sbt") project file:
```scala
libraryDependencies += "com.github.tminglei" %% "form-binder" % "0.9.0"
```

Then you can integrate it with your framework to simplify normal usage. 

Here's the way in my `Scalatra` project:

First, I defined a `FormBindSupport` trait,
```scala
trait MyFormBindSupport extends I18nSupport { self: ScalatraBase =>
  import MyFormBindSupport._

  before() {
    request(BindMessagesKey) = Messages(locale, bundlePath = "i18n/bind-messages")
  }

  def binder(implicit request: HttpServletRequest) =
    expandJsonString(Some("json")) >-: FormBinder(bindMessages.get).withErr(errsToJson4s)

  ///
  private def bindMessages(implicit request: HttpServletRequest): Messages = if (request == null) {
    throw new ScalatraException("There needs to be a request in scope to call bindMessages")
  } else {
    request.get(BindMessagesKey).map(_.asInstanceOf[Messages]).orNull
  }
}
```
Then mix it to my xxxServlet, and use it like this,
```scala
import com.github.tminglei.bind.simple._

class FeatureServlet extends ScalatraServlet with MyFormBindSupport {

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
```

_p.s. you can check more integration sample codes under  [/integrations](https://github.com/tminglei/form-binder/tree/master/integrations)._


Build & Test
-------------------
To hack it and make your contribution, you can setup it like this:
```bash
 $ git clone https://github.com/tminglei/form-binder.git
 $ cd form-binder
 $ sbt
...
```
To run the tests, pls execute:
```bash
 $ sbt test
```


Acknowledgements
-----------------
- [`Play!`](https://github.com/playframework/playframework) framework development team, for the original idea and implementation;
- [Naoki Takezoe](https://github.com/takezoe) and his [`scalatra-forms`](https://github.com/takezoe/scalatra-forms), a `play-data` implementation for scalatra.


License
---------
The BSD License, Minglei Tu &lt;tmlneu@gmail.com&gt;
