form-binder
===========
[![Build Status](https://travis-ci.org/tminglei/form-binder.svg?branch=master)](https://travis-ci.org/tminglei/form-binder)


Form-binder is a micro, `play-data`-like data binding and validating framework, easy to use and hack.

_* Form-binder was initially created for my scalatra-based project, but it can be easily integrated/used with other frameworks._


Features
-------------
- very lightweight, only 700+ lines (framework + built-in extensions)
- easy use, no verbose codes, and what you see is what you get
- high customizable, you can extend almost every executing point
- easily extensible, every extension interface is an alias of `FunctionN`
- immutable, you can share mapping definition object safely


Usage & Desc
-------------
![form-binder description](https://github.com/tminglei/form-binder/raw/master/form-binder-desc.png)

#### Major Components:  
[1] **binder**: facade, two major methods: `bind`, `validate`  
[2] **messages**: `(String) => String`, *(messageKey) => message*  
[3] **mapping**: used to validate/convert data  
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

#### Extension Points:  
(1) **BulkPreProcessor**: `(Map[String, String]) => Map[String, String]`, *data => data*  
(2) **PostErrProcessor**: `(Seq[(String, String)]) => R`, *errors => R*  
(3) **TouchedExtractor**: `(Map[String, String]) => Seq[String]`, *data => touched items*  
(4) **PreProcessor**: `(String) => String`, *(input) => output*  
(5) **Constraint**: `(String, String, Messages) => Option[String]`, *(label, vString, messages) => [error]*  
(6) **ExtraConstraint**: `(String, T, Messages) => Seq[(String, String)]`, *(label, vObject, messages) => errors*  

#### Options & Others:  
1) **label**: `feature`, readable name for current group/field  
2) **mapTo**: `feature`, map converted value to another type  
3) **eagerCheck**: `option`, check errors as more as possiable  
4) **ignoreEmpty**: `option`, not check empty field/values  
5) **touched**: `parameter`, a name list, which were touched by user  

_* By default, form-binder would return when encountered a validation error._  
_** ignoreEmpty + touched, will let form-binder re-check touched empty field/values_

For `form-binder` more details, pls check the [codes](https://github.com/tminglei/form-binder/tree/master/src/main/scala/com/github/tminglei/bind) and [tests](https://github.com/tminglei/form-binder/tree/master/src/test/scala/com/github/tminglei/bind).  
For `form-binder`/`Scalatra` integration, pls see [here](https://github.com/tminglei/form-binder/tree/master/integrations/scalatra).  


Install & Build
-------------------
The latest version of form-binder is `0.5.0`, and is published to [Maven Central](http://search.maven.org/):
```scala
libraryDependencies += "com.github.tminglei" %% "form-binder" % "0.5.0"
```

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
- `Play!` development team, for the original idea and implementation;
- [Naoki Takezoe](https://github.com/takezoe) and his [scalatra-forms](https://github.com/takezoe/scalatra-forms), a `play-data` implementation for scalatra.


License
---------
The BSD License, Minglei Tu &lt;tmlneu@gmail.com&gt;
