class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  // val numer = gcd(x/g) computed only once because of val

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < (that)) that else this

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  // def neg: Rational = new Rational(-numer, denom)
  def unary_- : Rational = new Rational(-numer, denom)

  // def - (that: Rational) = this + that.neg
  def - (that: Rational) = this + -that

  override def toString = numer + "/" + denom
}

val a = new Rational(1,3)
a.numer
a.denom

val b = new Rational(5,7)

a + b

val c = new Rational(3,2)

a - b - c

b + b

a < (b)

a max b

// val z = new Rational(1,0)

new Rational(2)