type Row = List[Int]
def Row(xs: Int*) = List(xs: _*)

type Matrix = List[Row]
def Matrix(xs: Row*) = List(xs: _*)

val m = Matrix(
  Row(1,2,3),
  Row(4,5,6),
  Row(1,2,3)
)

println(m)
println(m.getClass)

// Set
object anonymousSet {
  def contains(s: Int => Boolean, elem: Int): Boolean = s(elem)
  def singletonSet(elem: Int): Int => Boolean = {
    x: Int => x==elem
  }
  val A = singletonSet(5)
  println(contains(A,7))
}
anonymousSet


def contain(s: Int => Boolean, elem: Int): Boolean = s(elem)
def anonymousSet(elem: Int): Int => Boolean = {
  def inner(x: Int): Boolean = {
    x == elem
  }
  inner
}
val L = anonymousSet(5)
contain(L,7)

val M = anonymousSet(5)
contain(M,5)

anonymousSet(5)(5)

anonymousSet(5)(7)

def specialAnonymousSet(elem: Int)(a: Int): Boolean = {
  def inner(x: Int) : Boolean = {
    x == elem
  }
  inner(a)
}
specialAnonymousSet(5)(7)

specialAnonymousSet(5)(5)

// null
val x = null
val y: String = x

//Error:(60, 22) an expression of type Null is ineligible for implicit conversion
//val z: Int = null

if (true) 1 else false
