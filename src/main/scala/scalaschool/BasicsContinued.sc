// apply methods
class Foo {}

object FooMaker {
  def apply() = new Foo
}

val newFoo = FooMaker()

// or
class Bar {
  def apply() = 0
}

val bar = new Bar

bar()

// Objects
object Timer {
  var count = 0

  def currentCount(): Long = {
    count += 1
    count
  }
}

Timer.currentCount()

// Companion Object
class Bar2(foo: String)

object Bar2 {
  def apply(foo: String) = new Bar2(foo)
}

// Functions are Objects
object addOne extends Function1[Int, Int] {
  def apply(m: Int): Int = m + 1
}
addOne(1)

class AddOne extends Function1[Int, Int] {
  def apply(m: Int): Int = m + 1
}
val plusOne = new AddOne()
plusOne(1)

// short-hand
class AddOne2 extends (Int => Int) {
  def apply(m: Int): Int = m + 1
}

// Matching on values
val times = 1

times match {
  case 1 => "one"
  case 2 => "two"
  case _ => "some other number"
}

// Matching with guards
times match {
  case i if i == 1 => "one"
  case i if i == 2 => "two"
  case _ => "some other number"
}

// Matching on type
def bigger(o: Any): Any = {
  o match {
    case i: Int if i < 0 => i - 1
    case i: Int => i + 1
    case d: Double if d < 0.0 => d - 0.1
    case d: Double => d + 0.1
    case text: String => text + "s"
  }
}

class Calculator {
  val brand: String = "HP"
  val model: String = "10B"
  def add(m: Int, n: Int): Int = m + n
}

// Matching on class members
def calcType(calc: Calculator) = calc match {
  case _ if calc.brand == "HP" && calc.model == "20B" => "financial"
  case _ if calc.brand == "HP" && calc.model == "48G" => "scientific"
  case _ if calc.brand == "HP" && calc.model == "30B" => "business"
  case _ => "unknown"
}

// Case Classes
case class Calculator2(brand: String, model: String)

val hp20b = Calculator2("HP", "20b")
val hp20B = Calculator2("HP", "20b")

hp20b == hp20B

val hp30b = Calculator2("HP", "30B")

def calcType2(calc: Calculator2) = calc match {
  case Calculator2("HP", "20B") => "financial"
  case Calculator2("HP", "48G") => "scientific"
  case Calculator2("HP", "30B") => "business"
  case Calculator2(ourBrand, ourModel) => "Calculator: %s %s is of unknown type".format(ourBrand, ourModel)
}

// alternatives for last match
/*
case Calculator2(_, _) => "Calculator of unknown type"
case _ => "Calculator of unknown type"
*/

//Exceptions
/*
try {
  remoteCalculatorService.add(1, 2)
} catch {
  case e: ServerIsDownException => log.error(e, "the remote calculator service is unavailable. should have kept your trusty HP.")
} finally {
  remoteCalculatorService.close()
}
*/


// tryS are also expression-oriented
/*
val result: Int = try {
  remoteCalculatorService.add(1, 2)
} catch {
  case e: ServerIsDownException => {
    log.error(e, "the remote calculator service is unavailable. should have kept your trusty HP.")
    0
  }
} finally {
  remoteCalculatorService.close()
}
*/
