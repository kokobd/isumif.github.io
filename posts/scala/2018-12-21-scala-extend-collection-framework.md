---
title: Extending Scala's collection framework
---

## 增加一个新的IndexedSeq

实现trait `IndexedSeqLike`，并覆写`newBuilder`方法，可以改变take等方法的返回值类型

提供一个`CanBuildFrom[From, Elem, To]`类型的implicit值，可以改变map等方法的返回值类型

示例代码（一个存储RNA序列的集合）
```scala
import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}

final class RNA private (
    val groups: Array[Int],
    val length: Int
) extends IndexedSeq[Base]
    with IndexedSeqLike[Base, RNA] {

  import RNA._

  override def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx)
      throw new IndexOutOfBoundsException
    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }

  override protected[this] def newBuilder: mutable.Builder[Base, RNA] =
    RNA.newBuilder
}

object RNA {
  private val S = 2
  private val N = 32 / S
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- buf.indices) {
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    }
    new RNA(groups, buf.length)
  }

  def newBuilder: mutable.Builder[Base, RNA] =
    new mutable.ArrayBuffer mapResult fromSeq

  def apply(bases: Base*): RNA = {
    fromSeq(bases)
  }

  implicit def canBuildFrom: CanBuildFrom[RNA, Base, RNA] =
    new CanBuildFrom[RNA, Base, RNA] {
      def apply(): mutable.Builder[Base, RNA] = newBuilder
      def apply(from: RNA): mutable.Builder[Base, RNA] = newBuilder
    }
}
```

## 增加一个新的Map

和前面不同的是：
- 可以不覆写newBuilder，因为MapLike提供了这个方法的实现。
- 需要覆写empty方法


示例代码
```scala
import scala.collection.generic
import scala.collection.{mutable, immutable}

class PrefixMap[T]
    extends mutable.Map[String, T]
    with mutable.MapLike[String, T, PrefixMap[T]] {

  private var suffixes: Map[Char, PrefixMap[T]] = immutable.Map.empty
  private var value: Option[T] = None

  override def +=(kv: (String, T)): this.type = {
    update(kv._1, kv._2)
    this
  }

  override def -=(key: String): this.type = {
    remove(key)
    this
  }

  override def get(key: String): Option[T] = {
    if (key.isEmpty) value
    else suffixes.get(key(0)).flatMap(_.get(key substring 1))
  }

  override def iterator: Iterator[(String, T)] = {
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))
  }
  override def empty: PrefixMap[T] =
    PrefixMap.empty
}

object PrefixMap extends {
  def empty[T]: PrefixMap[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs)
      m += kv
    m
  }

  private type Builder[T] = mutable.Builder[(String, T), PrefixMap[T]]
  private type CanBuildFrom[T] =
    generic.CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]]

  private def newBuilder[T]: mutable.Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]: CanBuildFrom[T] = {
    new CanBuildFrom[T] {
      override def apply(from: PrefixMap[_]): Builder[T] =
        newBuilder[T]
      override def apply(): Builder[T] =
        newBuilder[T]
    }
  }
}
```
