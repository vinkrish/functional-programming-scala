// Basic Data Structures

// Arrays
val numArray = Array(1,2,3,4,5,1,2,3,4,5)

numArray(3)

// Lists
val numList = List(1,2,3,4,5,1,2,3,4,5)

numList(3)

// Sets
val numSet = Set(1,2,3,4,5,1,2,3,4,5)

numSet(3)

//Tuple
val hostPort = ("localhost", 80)

hostPort._1

hostPort._2

hostPort match {
  case ("localhost", port) => ""
  case (host, port) => ""
}

1 -> 2

// Maps
Map(1 -> 2)
Map("foo" -> "bar")

// Maps can themselves contain Maps or even functions as values.
def timesTwo(i: Int): Int = {
  println("Hello World!")
  i * 2
}

Map(1 -> Map("foo" -> "bar"))
Map("timesTwo" -> { timesTwo(_) })

// Option
val numbersOp = Map("one" -> 1, "two" -> 2)

numbersOp.get("two")

numbersOp.get("three")

// We want to multiply the number by two, otherwise return 0.
val result = if (res12.isDefined) {
  res12.get * 2
} else {
  0
}

val result2 = res12.getOrElse(0) * 2

val result3 = res12 match {
  case Some(n) => n * 2
  case None => 0
}

// Functional Combinators

// map
val numbers = List(1, 2, 3, 4)
numbers.map((i: Int) => i * 2)

def times2(i: Int): Int = i * 2
numbers.map(times2)

// foreach
numbers.foreach((i: Int) => i * 2)

val doubled = numbers.foreach((i: Int) => i * 2)

// filter
numbers.filter((i: Int) => i % 2 == 0)

def isEven(i: Int): Boolean = i % 2 == 0

numbers.filter(isEven)

// zip
List(1, 2, 3).zip(List("a", "b", "c"))

// partition
val numberss = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
numberss.partition(_ % 2 == 0)

// find
numberss.find((i: Int) => i > 5)

// drop and dropWhile
numberss.drop(7)

numberss.dropWhile(_ % 2 != 0)

// foldLeft
numberss.foldLeft(0)((m: Int, n: Int) => m + n)

numberss.foldLeft(0) { (m: Int, n: Int) => println("m: " + m + " n: " + n); m + n }

// foldRight
numberss.foldRight(0) { (m: Int, n: Int) => println("m: " + m + " n: " + n); m + n }

// flatten
List(List(1, 2), List(3, 4)).flatten

// flatMap
val nestedNumbers = List(List(1, 2), List(3, 4))

nestedNumbers.flatMap(x => x.map(_ * 2))

// Generalized functional combinators
def ourMap(numbers: List[Int], fn: Int => Int): List[Int] = {
  numbers.foldRight(List[Int]()) { (x: Int, xs: List[Int]) =>
    fn(x) :: xs
  }
}

ourMap(numberss, times2(_))

// Map?
val extensions = Map("steve" -> 100, "bob" -> 101, "joe" -> 201)

extensions.filter((namePhone: (String, Int)) => namePhone._2 < 200)