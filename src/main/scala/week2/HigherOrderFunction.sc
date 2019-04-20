// sum of integers between a and b
def sumInts(a: Int, b:Int): Int = {
  if (a > b) 0
  else a + sumInts(a+1, b)
}
sumInts(3,5)

// sum of cubes of all integers between a and b
def cube(a: Int): Int = a * a * a

def sumCubes(a: Int, b:Int): Int = {
  if (a > b) 0
  else cube(a) + sumCubes(a+1, b)
}
sumCubes(3,5)

// summing with higher order functions
def sums(f: Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sums(f, a+1, b)
}

def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

def sumFactorials(a: Int, b: Int): Int = sums(fact, a, b)

sumFactorials(3,5)

// tail-recursive version of sum
def summed(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a,0)
}

summed(x => x * x, 3, 5)

// Currying
// Functions Returning Functions
def sumC(f: Int => Int): (Int, Int) => Int = {
  def loop(a: Int, b: Int): Int = {
    if(a > b) 0
    else f(a) + loop(a + 1, b)
  }
  loop // loop is returned from sumC
}

def sumFactorial = sumC(fact)

sumFactorial(3,5)

// avoiding the middlemen i.e sumFactorial
sumC(fact)(3,5) // (sumC(fact))(3,5)

// special syntax for function returning a function
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a,0)
}
sum(x => x * x)(3, 5)
sum(fact)(3,5)

// Exercise
// product function
def prod(a: Int, b:Int): Int = {
  if (a > b) 1
  else a * prod(a+1, b)
}
prod(3,5)

// product with higher order function
def products(f: Int => Int)(a: Int, b:Int):Int = {
  if (a > b) 1
  else f(a) * products(f)(a+1, b)
}
products(x => x * x)(3,5)

def product(f: Int => Int)(a: Int, b:Int):Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else loop(a + 1, f(a) * acc)
  }
  loop(a, 1)
}
product(x => x)(3,5)

// factorial in terms of product
def factorialProduct(n: Int) = products(x => x)(1, n)

factorialProduct(5)

def productFactorial(f: Int => Int) (a: Int):Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a == 0) acc
    else loop(a - 1, f(a) * acc)
  }
  loop(a, 1)
}
productFactorial(x => x)(5)

// using for both sum and product
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, oneOrZero: Int)(a: Int, b: Int): Int = {
  if (a > b) oneOrZero
  else combine(f(a), mapReduce(f, combine, oneOrZero)(a+1, b))
}

def flexiProduct(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x*y, 1)(a, b)
flexiProduct(x => x*x)(3,4)

def flexiAdd(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x+y, 0)(a, b)
flexiAdd(x => x*x*x)(3,5)

