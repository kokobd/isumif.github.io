---
title: Haskell三层设计模式
---

不少人认为Haskell没有设计模式，所谓“设计模式”已经用语言本身的性质（例如immutability）以及各种抽象库实现了。但是，对于如何在应用程序中设计单子栈，在web应用中如何处理业务逻辑、数据库操作、http handler等等之间的边界，我之前是比较模糊的。我使用过的一种做法是：对每部分独立设计一个单子栈，再编写函数用于转换不同的单子环境。结果每个单子栈都过于复杂，代码也难以测试

下面介绍一种设计方法（[原文链接][1]），非常实用，而且被较多人采纳。

[1]: https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html