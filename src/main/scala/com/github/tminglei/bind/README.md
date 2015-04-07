How it works
--------------------
### Principle
The core of `form-binder` is `Mapping`, **tree structure** mappings. With **depth-first** algorithm, it was used to validate data and construct the result value object.

### Details

![form-binder description](https://github.com/tminglei/form-binder/raw/master/form-binder-desc.png)

#### Major Components:  
[1] **binder**: facade, two major methods: `bind`, `validate`  
[2] **messages**: `(String) => Option[String]`, *(messageKey) => [message]*  
[3] **mapping**: validate/convert data, two types of mappings: `FieldMapping` and `GroupMapping`  
[4] **data**: `Map[String, String]`  

binder **bind** method signature:
```scala
//if validation passed, consume will be executed, and return `R2`; if validation failed, 
//return errors map or user transformed errors
def bind[T, R2](mapping: Mapping[T], data: Map[String, String])(consume: T => R2)
```

binder **validate**, _validate only_ and not consume converted data, method signature:
```scala
//return errors map or user transformed errors
def validate[T](mapping: Mapping[T], data: Map[String, String], touched: Option[Seq[String]] = None)
```

> _Check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Mappings.scala) for built-in **mapping**s._  

#### Extension Points:  
(1) **PreProcessor**: `(String, Map[String, String], Options) => Map[String, String]`, *(prefix, data, options) => data*  
(2) **PostErrProcessor**: `(Seq[(String, String)]) => R`, *errors => R*  
(3) **TouchedExtractor**: `(Map[String, String]) => Seq[String]`, *data => touched items*  
(4) **Constraint**: `(String, Map[String, String], Messages, Options) => Seq[(String, String)]`, *(name, data, messages, options) => errors*  
(5) **ExtraConstraint**: `(String, T, Messages) => Seq[(String, String)]`, *(label, vObject, messages) => errors*  

> _* Check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Processors.scala) for built-in `PreProcessor`/`TouchedExtractor`/`PostErrProcessor`._  
> _**Check [here](https://github.com/tminglei/form-binder/blob/master/src/main/scala/com/github/tminglei/bind/Constraints.scala) for built-in `Constraint`._

#### Options & Others:  
1) **label**: `feature`, readable name for current group/field  
2) **mapTo**: `feature`, map converted value to another type  
3) **i18n**: `option`, let label value can be used as a message key to fetch a i18n value from `messages`   
4) **eagerCheck**: `option`, check errors as more as possible  
5) **ignoreEmpty**: `option`, not check empty field/values  
6) **touched**: `parameter`, a name list, which were touched by user  

> _* By default, form-binder would return right after encountered a validation error._  
> _** ignoreEmpty + touched, will let form-binder re-check touched empty field/values._
