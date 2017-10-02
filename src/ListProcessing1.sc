object ListProcessing1 {
  // problem 1, 2, 3, 6, 7, 8, 10, 13


  // problem 1

  // tests:
  //**************

  // problem 2

  // tests:
  //**************

  // problem 3
  // A function that returns the depth of a list of nested lists.
  def depthRecur(list: Any) : Int = {
    list match {
      case (first :: rest) => math.max(depthRecur(first) + 1, depthRecur(rest))
      case _ => 0 // not a list so return 0
    }
  }
  def depthIter(vals: Any) : Int = {

    var result = 0

    while(vals.isInstanceOf[List[Any]]) {
//      if (vals.isInstanceOf[List[Any]]) {
        val valsList = vals.asInstanceOf[List[Any]]
        for (v <- valsList) {
          result = math.max(result, depthIter(valsList.head))
        }
//      }
    }
    result
  }

  def depthMapFilterReduce(vals: Any) = {

  }

  // tests:
  depthRecur(List(List(List (1, 2, List(3)))))
  depthRecur(List(1))
  //**************

  // problem 6

  // tests:
  //**************

  // problem 7
  // A function that returns true if all elements
  // in a lists satisfy a given predicate
  def allTailRecur[T](test: T=>Boolean, vals: List[T]) : Boolean = {
    if (vals == Nil)
      true
    else if (test(vals.head))
      allTailRecur(test, vals.tail)
    else
      false

  }
  def allIter[T](test: T=>Boolean, vals: List[T]) : Boolean = {
    var result = true
    for (v <- vals if result) {
      result = result && test(v)
    }
    result
  }
  // tests:
  def isPal(s: String) = s == s.reverse
  def isEven(n: Int) = n % 2 == 0
  allTailRecur(isPal, List("mom", "rotator", "dad"))
  allTailRecur(isEven, List(2,4,6,7))

  allIter(isPal, List("mom", "rotator", "dad"))
  allIter(isEven, List(2,3,4,6,8,10))
  //**************

  // problem 8

  // tests:
  //**************

  // problem 10

  // tests:
  //**************

  // problem 13
  // 1. An infinitely long stream of 1's
  // 2. The stream of all non-negative integers
  // by reverse recusion
  def makeNaturalNumber(from: Int) : Stream[Int] =
  from #:: makeNaturalNumber(from + 1)
  val naturalNumber = makeNaturalNumber(0)
  naturalNumber
  naturalNumber.head
  naturalNumber.tail
  naturalNumber(5)
  naturalNumber // forcing the promise
  // lecture example
  def isPrime(n: Int) = {
    if (n < 2)
      false
    else {
      var result = true
      for (i <- 2 until n / 2 if result) {
        result = n % i != 0
      }
      result
    }
  }
  val primes = naturalNumber.filter(isPrime)
  primes(3)
  // end of lecture example
  // 3. The stream of all non-negative even integers
  // 4. The stream of all squares of integers
  // tests:
  //**************


}