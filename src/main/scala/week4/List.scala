package week4

import java.util.NoSuchElementException

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  // violates variance checking rule
  // def prepend(elem: T): List[T] = new Cons(elem, this)
  def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
  /*
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y prepend ys => if (x <= y) x prepend xs else y prepend insert(x, ys)
  }
  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y prepend ys => insert(y, isort(ys))
  }
  */
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing =  throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object test {
  val x: List[String] = Nil
  // def f(xs: List[NonEmpty], x: Empty) = xs prepend x
}