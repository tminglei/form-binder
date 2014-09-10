form-binder
===========
[![Build Status](https://travis-ci.org/tminglei/form-binder.svg?branch=master)](https://travis-ci.org/tminglei/form-binder)


`form-binder` is a general form data binding and validating framework inspired by `play-data`, very easy to use and hack.

`form-binder` was initially created for my scalatra-based project, but you can use it in other scenarios, since it didn't depend on scalatra codes, and easy to integrate with other frameworks.


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
The core of `form-binder` is a **composite mapping**,
- when fed a data map, top/parent mapping will direct child mappings to start their jobs
- child or child of child mappings will get their string value from the data map, and do the validation or convertion
- their parent mapping will continue: return/report found errors from children; or do more data checking if necessary, then return; or make a case class or tuple, then return to its parent
- finnaly, we got an error sequence if validation errors found, or a full case class or tuple
- if data is valid, our consume codes will be fed an expected object, then do our jobs

_**p.s. `form-binder` will validate data firstly; if errors found, stop and report; if no errors found, then convert to result object and execute user logics.**_

.
Next, I'll explain its components and details based on above **raw** usage codes:

### _bind.simple_
```scala
import com.github.tminglei.bind.simple._
```
`bind.simple` is a helper object, let us use form binder's built-in mappings/constraints/processors directly 

### Messages, BulkPreProcessor and PostErrProcessor
```scala
val binder = expandJsonData("json") pipe_: FormBinder(messages).withErr(errsToJson4s)
```
It defined a form binder with Messages, BulkPreProcessor and PostErrProcessor

#### Messages
`Messages` is an alias of `(String) => String`, used to provide message template to validation errors. Dev user is required to provide `Messages` implementation.
```scala
// (messageKey) => message
type Messages = (String) => String
```

#### BulkPreProcessor
`BulkPreProcessor` is an alias of `(Map[String, String]) => Map[String, String]`, used to bulk pre process data map. _What I thought about are expanding a json string to series of data items, or merging existing json data to data map._
```scala
// (data) => data
type BulkPreProcessor = (Map[String, String]) => Map[String, String]
```

#### PostErrProcessor
`PostErrProcessor` is an alias of `(Seq[(String, String)]) => R`, used to process error sequence, _e.g. transforming errors to json value_.
```scala
// (errors) => R
type PostErrProcessor[R] = (Seq[(String, String)]) => R
```

For built-in `BulkPreProcessor` and `PostErrProcessor` implementations/details, pls see [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Processors.scala#L40).  
_**Please implement yours if necessary.**_

### Mapping, PreProcessor and Constraint/ExtraConstraint
```scala
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
```
It defined a composite mapping with pre-processors and constraints.

#### Mapping
A `mapping` is a binding to series of constraints and processors/conversion. There are majorly two types of mappings: FieldMapping and GroupMapping.

A **FieldMapping** corresponds to a field, a simple value; is a leaf in the composite mapping tree
A **GroupMapping** corresponds to a group, be made of series of name-mapping pairs; is a branch/trunk in the composite mapping tree

`form-binder` built-in defined `text`, `boolean`, `number`, `long`, `double`, `date` and so on for `FieldMapping`; defined group mappings for 1~22 members, both case class and tuple can be used as result types.

> Use `mapping` if result type is a case class, the codes will be like: `mapping(...)(Order.apply)`  
> Use `tmapping` if result type is a tuple, the codes will be like: `tmapping(...)`

In fact, there is third type of mappings: **general usage mappings**.
Currently, `form-binder` built-in general usage mappings include:
- ignored
- optional
- default
- list
- seq
- map

For built-in `Mapping` implementations/details, pls see [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Mappings.scala).  
_**Please implement yours if necessary.**_

#### PreProcessor
A `FieldMapping` can associate one or more `PreProcessor`, to help clean/adjust string data, e.g. cleaning '$' from '$137.5', removing ',' from '24,567'.

For built-in `PreProcessor` implementations/details, pls see [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Processors.scala).  
_**Please implement yours if necessary.**_

#### Constraint
A `Constraint` checks a single raw string value, to find validation errors, which are always assocated to a `FieldMapping` through its construct method.

#### ExtraConstraint
A `ExtraConstraint` checks on a converted value, to find validation errors, which are always assocated to a Mapping through its `verifying` method.
_(NOTE: `ExtraConstraint` can be associated to both `GroupMapping` and `FieldMapping`)._

For built-in `Constraint`/`ExtraConstraint` implementations/details, pls see [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Constraints.scala).  
_**Please implement your if necessary.**_

### FormBinder and bind
```scala
val data = Map(
  "id" -> "133",
  "json" -> """{"name":"xxxx", "email":"123@example.com", "price":"$137.5", "count":5}"""
)
binder.bind(mappings, data) { case (id, order) =>
  //do something ...
}
```
It defined a mock data map, and binder bind it to the mapping, with result object consuming codes.

Here the json string in the data map will be expanded by BuldPreProcessor `expandJsonData("json")` to:
```scala
"json.name"  -> "xxxx",
"json.email" -> "123@example.com",
"json.price" -> "$137.5",
"json.count" -> "5"
```
Then were verified and converted by mapping:
```scala
  "json" -> mapping(
    "name"  -> text(maxlength(100)),
    "email" -> text(email()),
    "price" -> (cleanPrefix("$") pipe_: float()),
    "count" -> number().verifying(min(3))
  )(Order.apply).label("order")
```

`binder.bind` accepts mapping and data map as its first part parameters, and `consume: T => R2` as its second part parameter; if no errors found, `consume` method as below will be executed:
```scala
{ case (id, order) =>
  //do something ...
}
```
`binder.bind` return a `R2` type value produced by `consume` if validation passed; or errors (maybe transformed) if validation failed.

For more implementation details, pls check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Framework.scala).

### Others
#### label
A mapping's label can replace its full path name to be used in error messages.

Without `label`, an error message formated with path name may be like this: "json is required", "json.price can't be greater then 100".
But if you associated a label at proper place, you can get messages like this: "order is required", "product price can't be greater than 100".

_You can associate a mapping with its `.label(..)` method, if it support that._

#### mapTo
`Mapping.mapTo` method can transform an original result object to another.

A typical use case is: you used some numbers to represent a series status, which can be applied `&` and `|` operations, and then you wrapped them with a wrapper class, since it can bring up type safe benefits.

Then you can define its validation rules and conversion logic like this:
```scala
"xxx" -> number(oneOf(1,2,4)).mapTo(WrapClass _)
```

_That's all. If not understand it yet, you can try to read the source codes. It's only 700 lines or so._
_**Any problem, pls file an issue. I'll resolve it asap.**_


Install & Build
-------------------
To use `form-bind` in `sbt` managed project, add the following to your project file:
```scala
libraryDependencies += "com.github.tminglei" %% "form-binder" % "0.3.0"
```
Or, in `maven` project, you add `form-binder` to your `pom.xml` like this:
```
<dependency>
    <groupId>com.github.tminglei</groupId>
    <artifactId>form-binder_2.11</artifactId>
    <version>0.3.0</version>
</dependency>
```

To hack it and make your contribution, you can setup it like this:
```bash
tminglei@tt500 ~/repos $ git clone https://github.com/tminglei/form-binder.git
tminglei@tt500 ~/repos $ cd form-binder
tminglei@tt500 ~/repos/form-binder $ sbt
...
```
To run the tests, pls execute:
```bash
tminglei@tt500 ~/repos/form-binder $ sbt test
```

Acknowledgements
-----------------
Firstly, I want to thank `Play!` development team. Except them, I also want to say thanks to
- [Naoki Takezoe](https://github.com/takezoe) and his [scalatra-forms](https://github.com/takezoe/scalatra-forms), which provide a simplified/improved `play-data` implementation for scalatra, and many useful requirement points;
- [Cody Allen](https://github.com/ceedubs) and his [scrutinator](https://github.com/ceedubs/scrutinator), which showed to me an elegant `shapeless` and `ScalaCheck` usage, and data binding definition and swagger param declaration merging.

**I got much from your works. Thank you all very much!**

License
---------
The BSD License, Minglei Tu &lt;tmlneu@gmail.com&gt;
