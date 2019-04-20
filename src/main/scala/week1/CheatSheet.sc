// Evaluation Rules

def example1 = 2      // evaluated when called

val example2 = 2      // evaluated immediately

lazy val example3 = 2 // evaluated once when needed

// Higher Order Functions

// sum() returns a function that takes two integers and returns an integer
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumf(a: Int, b: Int): Int = { a + b }
  sumf
}

val fruitList = List("apples", "oranges", "pears")

// Alternative syntax for lists
val fruit = "apples" :: ("oranges" :: ("pears" :: Nil)) // parens optional, :: is right-associative
fruit.head   // "apples"
fruit.tail   // List("oranges", "pears")

val empty1 = List()
val empty2 = Nil

val nums = Vector("louis", "frank", "hiromi")
nums(1)                     // element at index 1, returns "frank", complexity O(log(n))
nums.updated(2, "helena")   // new vector with a different string at index 2, complexity O(log(n))

val fruitSet = Set("apple", "banana", "pear", "banana")
fruitSet.size    // returns 3: there are no duplicates, only one banana

val r: Range = 1 until 5 // 1, 2, 3, 4
val s: Range = 1 to 5    // 1, 2, 3, 4, 5
1 to 10 by 3  // 1, 4, 7, 10
6 to 1 by -2  // 6, 4, 2

val ss = (1 to 6).toSet
ss map (_ + 2) // adds 2 to each element of the set

val sss = "Hello World"
sss filter (c => c.isUpper) // returns "HW"; strings can be treated as Seq[Char]

// Operations on sequences
val xs = List(1,2,3)
xs.length   // number of elements, complexity O(n)
xs.last     // last element (exception if xs is empty), complexity O(n)
xs.init     // all elements of xs but the last (exception if xs is empty), complexity O(n)
xs take 1   // first n elements of xs
xs drop 2   // the rest of the collection after taking n elements
xs(1)       // the nth element of xs, complexity O(n)

// Operations on maps
val myMap = Map("I" -> 1, "V" -> 5, "X" -> 10)  // create a map
myMap("I")      // => 1
myMap("A")      // => java.util.NoSuchElementException
myMap get "A"   // => None
myMap get "I"   // => Some(1)
myMap.updated("V", 15)  // returns a new map where "V" maps to 15 (entry is updated)
// if the key ("V" here) does not exist, a new entry is added

// Operations on Streams
val xs2 = Stream(1, 2, 3)
val xs3 = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))) // same as above
(1 to 1000).toStream // => Stream(1, ?)
// x #:: xs3 // Same as Stream.cons(x, xs)
// In the Stream's cons operator, the second parameter (the tail)
// is defined as a "call by name" parameter.
// Note that x::xs always produces a List