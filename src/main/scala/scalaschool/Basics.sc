// Expressions
1+1

// Values
val two = 1+1

//Variables
var name = "Vinay"
name = "Krishna"

// Functions
def addOne(m: Int): Int = m + 1

val three = addOne(2)

def three() = 1 + 2

three()

three

// Anonymous Functions
(x: Int) => x + 1

res3(1)
res3(2)

val addOne = (x: Int) => x + 1
addOne(1)

def timesTwo(i: Int): Int = {
  println("Hello World!")
  i * 2
}

{
  i: Int =>
    println("Hello Vinay!")
    i * 2
}

// Partial application
def adder(m: Int, n: Int) = m + n

val add2 = adder(2, _:Int)

add2(3)

// Curried Functions
def multiply(m: Int)(n: Int): Int = m * n

multiply(2)(3)

val timesTwo = multiply(2)_

timesTwo(3)

// function of multiple arguments & curry it
val curriedAdd = (adder _).curried

val addTwo = curriedAdd(2)

addTwo(4)

// Variable length arguments
def capitalizeAll(args: String*) = {
  args.map { arg =>
    arg.capitalize
  }
}

capitalizeAll("rarity", "applejack")

// Classes
class Calculator {
  val brand: String = "HP"
  def add(m: Int, n: Int): Int = m + n
}

val calc = new Calculator

calc.add(1, 2)

calc.brand

// Constructor
class Calculator2(brand: String) {
  val color: String = if (brand == "TI") {
    "blue"
  } else if (brand == "HP") {
    "black"
  } else {
    "white"
  }

  // An instance method.
  def add(m: Int, n: Int): Int = m + n
}

val calc2 = new Calculator2("HP")

calc2.color

// Functions vs Methods
class C {
  var acc = 0
  def minc = { acc += 1}
  val finc = { () => acc += 1 }
}

val c = new C

// calls c.minc()
c.minc

// returns the function as a value
c.finc

// Inheritance
class ScientificCalculator(brand: String) extends Calculator2(brand) {
  def log(m: Double, base: Double) = math.log(m) / math.log(base)
}

// Overloading methods
class EvenMoreScientificCalculator(brand: String) extends ScientificCalculator(brand) {
  def log(m: Int): Double = log(m, math.exp(1))
}

// abstract Classes
abstract class Shape {
  def getArea(): Int
}

class Circle(r: Int) extends Shape {
  def getArea(): Int = { r * r * 3 }
}

val c = new Circle(2)

// Traits
trait Car {
  val brand: String
}

trait Shiny {
  val shineRefraction: Int
}

class BMW extends Car {
  val brand = "BMW"
}

// One class can extend several traits using the with keyword
class BMW2 extends Car with Shiny {
  val brand = "BMW"
  val shineRefraction = 12
}

// Types
trait Cache[K, V] {
  def get(key: K): V
  def put(key: K, value: V)
  def delete(key: K)
}