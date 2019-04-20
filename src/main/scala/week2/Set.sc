
// Purely Functional Sets

type Setter = Int => Boolean

def contains(s: Setter, elem: Int): Boolean = s(elem)

def singleSet(elem: Int): Setter = Set(elem)

def singletonSet(x: Int): Setter = {
  elem: Int => if (elem == x) true else false
}

val A = singleSet(5)

contains(A,7)

val B = singletonSet(5)

contains(B,5)

val bound = 1000
def toString(s: Setter): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}

println(B.toString())
println(B)

def toInt(s: Setter): Any = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.head
}
println(toInt(B))

val mulSet = Set(1,1,3)
contains(mulSet, 5)
contains(mulSet, 3)

val s1 = singletonSet(1)
val s2 = singletonSet(2)

def union(s: Setter, t: Setter): Setter = {
  x => if ( contains(s, x) || contains(t, x)) true else false
}
val unionSet = union(s1, s2)
contains(unionSet, 1)
contains(unionSet, 2)
contains(unionSet, 3)

def intersect(s: Setter, t: Setter): Setter = {
  x => if (contains(s, x) && contains(t, x)) true else false
}
val si1 = singletonSet(1)
val si2 = singletonSet(1)
val intersectSet = intersect(si1, si2)
contains(intersectSet, 1)
contains(intersectSet, 2)

val interSet = intersect(s1, s2)
contains(interSet, 1)
contains(interSet, 2)

def diff(s: Setter, t: Setter): Setter = {
  x => if (contains(s, x) && !contains(t,x)) true else false
}
val diffSet = diff(s1, s2)
contains(diffSet, 1)
contains(diffSet, 2)

def filter(s: Setter, p: Int => Boolean): Setter = {
  x => if (contains(s, x) && p(x)) true else false
}
val unionFilter = union(s1, s2)
val filterSet = filter(unionFilter, a => if (a%2 == 0) true else false )
contains(filterSet, 1)
contains(filterSet, 2)
contains(filterSet, 4)

def forall(s: Setter, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a == 1001) true
    else if (contains(s, a) && !p(a)) false
    else iter(a+1)
  }
  iter(-1000)
}

def exists(s: Setter, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a == 1001) false
    else if (contains(s, a) && p(a)) true
    else iter(a+1)
  }
  iter(-1000)
}

// map
def m1(x: Int): Boolean = x == 1 || x == 2 || x == 3
def f(x: Int) : Int = x * 2
def m2(x: Int): Boolean = x == 2 || x == 4 || x == 6