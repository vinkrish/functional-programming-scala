abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString: String = "{" + left + elem + right + "}"
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def union(other: IntSet): IntSet = other
  override def toString = "."
}

val t1 = new NonEmpty(3, new Empty, new Empty)
val t2 = t1 incl 4
t2 contains 3

val u1 = new NonEmpty(5, new Empty, new Empty)
val u2 = u1 incl 3
val u3 = u2 incl 7
val u4 = u3 incl 6

t2 union u4

u4 union t2

