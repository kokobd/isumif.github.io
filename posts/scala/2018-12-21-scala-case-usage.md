---
title: Usage of 'case' in Scala
---

`case`可以在3个地方使用：

和`match`一起用：
```scala
  obj match {
    case pattern if guard => statements
    case pattern => statements
  }
```

作为(partial) function literal
```scala
{ // 注意必须使用花括号而不是圆括号
    case pattern if guard => statements
    case pattern => statements
}
```

和`catch`一起使用：
```scala
try {
    block
} catch {
    case pattern if guard => statements
    case pattern => statements
} finally {
    block
}
```

---

所以有这种写法：
```
listOfTuples.map{case (x, y) => ???}
```
因为如果函数调用时参数列表中只有一个参数，就可以换成花括号，然后正好可以在里面写`case`，把它作为function literal