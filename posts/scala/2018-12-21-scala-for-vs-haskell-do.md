---
title: Scala 'for' vs Haskell 'do'
---

|Haskell|Scala|
|:-|:-|
|`x <- monadValue`|`x <- monadValue`|
|`monadValue`|`_ <- monadValue`|
|`let y = someExpr`|`val y = someExpr`|
|最后一行成为整个do表达式的值|必须使用`yield r`，则`pure r`成为整个表达式的的值|

例子：
Scala
```scala
for {
  x <- monadValue
  _ <- operation
  val y = someExpr
  r <- lastMonadValue
} yield r
```
Haskell
```haskell
do
  x <- monadValue
  operation
  let y = someExpr
  lastMonadValue
```
