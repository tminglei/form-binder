How it works
--------------------
### Principle
The core of `form-binder` is `Mapping`, **tree structure** mappings. With **depth-first** algorithm, it was used to validate data and construct the result value object.

### Details

![form-binder description](https://github.com/tminglei/form-binder/raw/master/form-binder-desc.png)

#### Major Components:  
[1] **binder**: facade, used to bind and trigger processing, two major methods: `bind`, `validate`  
[2] **messages**: used to provide error messages  
[3] **mapping**: holding constraints, processors, and maybe child mapping, etc. used to validate/convert data, two types of mappings: `field` and `group`  
[4] **data**: inputting data map  

> _Check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Framework.scala) for framework details._

binder **bind** method signature (return an `Either` and let user to continue processing):
```scala
//bind mappings to data, and return an either, holding validation errors (left) or converted value (right)
def bind[T, M <: InputMode](mapping: Mapping[T, M], data: Map[String, String]): Either[R, T]
```

binder **validate** method signature (_validate only_ and not consume converted data):
```scala
//return (maybe processed) errors
def validate[T](mapping: Mapping[T], data: Map[String, String], touched: Option[Seq[String]] = None)
```

> _Check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Mappings.scala) for built-in **mapping**s._  

#### Extension Types:  
(1) **PreProcessor**: used to pre-process data, like omitting `$` from `$3,013`  
(2) **ErrProcessor**: used to process error seq, like converting it to json  
(3) **Constraint**: used to validate raw string data  
(4) **ExtraConstraint**: used to valdate converted value  

> _* Check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Processors.scala) for built-in `PreProcessor`/`ErrProcessor`._  
> _**Check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Constraints.scala) for built-in `Constraint`/`ExtraConstraint`._

#### Options/Features:  
1) **label**: `feature`, readable name for current group/field  
2) **mapTo**: `feature`, map converted value to another type  
3) **i18n**: `option`, let label value can be used as a message key to fetch a i18n value from `messages`   
4) **eagerCheck**: `option`, check errors as more as possible  
5) **ignoreEmpty**: `option`, not check empty field/values, especially they're not touched by user  
6) **touched**: `parameter`, a name list touched by user; for these fields, they can't be empty if they're required  

> _* By default, form-binder would return right after encountered a validation error._  
> _** ignoreEmpty + touched, will let form-binder re-check touched empty field/values._

#### Others:
1) **InputMode** - logically, some constraints/processors can only process single input, and some can only process multiple input. To help user not to wrongly use them, `InputMode` was introduced. Extension developers can mix them to the constraint/processor definitions, and `Scala` compiler will help do the checking.
