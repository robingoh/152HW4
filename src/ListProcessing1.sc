object ListProcessing1 {
  // problem 1, 2, 3, 6, 7, 8, 10, 13
  // problem 1,2,6,7,8 implement 4 versions:
  // iterative, recursive, tail-recursive, map-filter-reduce


  // problem 1
  // A function that computes the sum of cubes of all odd
  // numbers occurring in a list of integers
  // iterative version
  def sumOfOddCubesIter(nums: List[Int]) = {
    var result = 0
    for (i <- nums if i % 2 != 0) result += i * i * i
    result
  }
  // recursive version
  def sumOfOddCubesRecur(nums: List[Int]) : Int = {
    if (nums == Nil) 0
    else if (nums.head % 2 != 0)
      nums.head * nums.head * nums.head + sumOfOddCubesRecur(nums.tail)
    else sumOfOddCubesRecur(nums.tail)
    }
  // tail-recursive version
  def sumOfOddCubesTailRecur(nums: List[Int]) = {
    def helper(result: Int, unseen: List[Int]) : Int = {
      if (unseen == Nil) result
      else if (unseen.head %2 != 0) helper(result + unseen.head * unseen.head * unseen.head, unseen.tail)
      else helper(result, unseen.tail)
    }
    helper(0, nums)
  }
  // map-filter-reduce version
  def isOdd(n: Int) = n % 2 != 0
  def cube(n: Int) = n * n * n
  def sum(n1: Int, n2: Int) = n1 + n2
  def sumOfOddCubesMapReduceFilter(nums: List[Int]) = nums.filter(isOdd _).map(cube _).reduce(sum _)
  // tests:
  val listToBeCubed = List(1,2,3,4,5,6,7,8,9)
  sumOfOddCubesIter(listToBeCubed)
  sumOfOddCubesRecur(listToBeCubed)
  sumOfOddCubesTailRecur(listToBeCubed)
  sumOfOddCubesMapReduceFilter(listToBeCubed)

  //**************

  // problem 2
  // A function that computes the sum of numbers in
  // a list of lists of numbers.
  // iterative version
  def sumOfSumsIter(listOfLists: List[List[Int]]) = {
    var result = 0
    for (list <- listOfLists)
      for (num <- list)
        result += num
    result
  }
  // recursive version
  def sumOfSumsRecur(listOfLists: List[List[Int]]) : Int = {
    if (listOfLists == Nil) 0
    else {
      if (listOfLists.head == Nil) sumOfSumsRecur(listOfLists.tail)
      else listOfLists.head.head + sumOfSumsRecur(listOfLists.head.tail::listOfLists.tail)
    }
  }

  // tail-recursive version
  def sumOfSumsTailRecur(listOfLists: List[List[Int]]) = {
    def helper(result: Int, unseen: List[List[Int]]) : Int = {
      if (unseen == Nil) result
      else if (unseen.head == Nil) helper(result, unseen.tail)
      else helper(result + unseen.head.head, unseen.head.tail::unseen.tail)
    }
    helper(0, listOfLists)
  }
  // map-filter-reduce version
  def sumAllNumInAList(list: List[Int]) = {
    var sum = 0
    for (num <- list) sum += num
    sum
  }
  def sumOfSumsMapFilterReduce(listOfLists: List[List[Int]]) = listOfLists.map(sumAllNumInAList _).reduce(sum _)
  // tests:
  val listOfListsToTest = List( List(1, 2, 3), List(4, 5, 6) )
  sumOfSumsIter(listOfListsToTest)
  sumOfSumsRecur(listOfListsToTest)
  sumOfSumsTailRecur(listOfListsToTest)
  sumOfSumsMapFilterReduce(listOfListsToTest)

  //**************

  // problem 3
  // A function that returns the depth of a list of nested lists.

  def depthRecur(list: Any) : Int = {
    list match {
      case (first :: rest) => math.max(depthRecur(first) + 1, depthRecur(rest))
      case _ => 0 // not a list so return 0
    }
  }

  // tests:
  depthRecur(List(List(List (1, 2, List(3)))))
  depthRecur(List(1))
  //**************

  // problem 6
  // A function that returns the nunmber of elements in a list that satisfy
  // a given predicate of type T=>Boolean
  // iterative version
  def numOfEleSatisfyPredicateIter[T](list: List[T], predicate: T=>Boolean) = {
    var count = 0
    for (element <- list)
      if (predicate(element)) count += 1
    count
  }
  // recursive version
  def numOfEleSatisfyPredicateRecur[T](list: List[T], predicate: T=>Boolean) : Int = {
    if (list == Nil) 0
    else if (predicate(list.head)) 1 + numOfEleSatisfyPredicateRecur(list.tail, predicate)
    else numOfEleSatisfyPredicateRecur(list.tail, predicate)
  }
  // tail-recursive version
  def numOfEleSatisfyPredicateTailRecur[T](list: List[T], predicate: T=>Boolean) = {
    def helper(result: Int, unseen: List[T]) : Int = {
      if (unseen == Nil) result
      else if (predicate(unseen.head)) helper(result + 1, unseen.tail)
      else helper(result, unseen.tail)
    }
    helper(0, list)
  }
  // map-filter-reduce version
  def numOfEleSatisfyPredicateMapFilterReduce[T](list: List[T], predicate: T=>Boolean) = list.filter(predicate).size
  // a more succient way is to do list.count(predicate _), but problem requires using map, filter and reduce


  // tests:
  def stringConsistOfExactly5Chars(str: String) = str.length == 5
  var listToTestForProb6 = List("Kodak", "nikon", "goPro", "canon", "Samsung", "iPhone")
  numOfEleSatisfyPredicateIter(listToTestForProb6, stringConsistOfExactly5Chars _) // expecting: 4
  numOfEleSatisfyPredicateRecur(listToTestForProb6, stringConsistOfExactly5Chars _) // expecting: 4
  numOfEleSatisfyPredicateTailRecur(listToTestForProb6, stringConsistOfExactly5Chars _) // expecting: 4
  numOfEleSatisfyPredicateMapFilterReduce(listToTestForProb6, stringConsistOfExactly5Chars _) // expecting: 4

  //**************

  // problem 7
  // A function that returns true if all elements
  // in a lists satisfy a given predicate.
  // iterative version
  def allIter[T](test: T=>Boolean, vals: List[T]) = {
    var result = true
    for (v <- vals if result)
      result = result && test(v)
    result
  }
  // recursive version
  // ** the intuitive recursive version is the same as the tail-recursive version
  // ** since the problem requires a recursive version, I have done something
  // ** to differentiate the recursive and the tail-recursive version. But in the
  // ** end, allRecur and allTailRecur are both tail-recursive functions.
  // ** In other words, no recursive version of problem 7 is found.
  def allRecur[T](test: T=>Boolean, vals: List[T]) : Boolean = {
    if (vals == Nil) true
    else test(vals.head) && allRecur(test, vals.tail)
  }
  // tail-recursive version
  def allTailRecur[T](test: T=>Boolean, vals: List[T]) : Boolean = {
    if (vals == Nil) true
    else if (test(vals.head)) allTailRecur(test, vals.tail)
    else false
  }
  // map-filter-reduce version
  // using vals.count(test) would be more direct, but again, the problem
  // requires using only map, filter and reduce.
  def allMapFilterReduce[T](test: T=>Boolean, vals: List[T]) = vals.size == vals.filter(test).size


  // tests:
  def isPal(s: String) = s == s.reverse
  def isEven(n: Int) = n % 2 == 0
  val listAToTestForProb7 = List("mom", "rotator", "dad")
  val listBToTestForProb7 = List("mom", "rotator", "daddy")
  val listCToTestForProb7 = List(2,3,4,6,8,10)
  val listDToTestForProb7 = List(2,4,6,8)
  allIter(isPal _, listAToTestForProb7)
  allIter(isPal _, listBToTestForProb7)
  allIter(isEven _, listCToTestForProb7)
  allIter(isEven _, listDToTestForProb7)

  allRecur(isPal _, listAToTestForProb7)
  allRecur(isPal _, listBToTestForProb7)
  allRecur(isEven _, listCToTestForProb7)
  allRecur(isEven _, listDToTestForProb7)

  allTailRecur(isPal _, listAToTestForProb7)
  allTailRecur(isPal _, listBToTestForProb7)
  allTailRecur(isEven _, listCToTestForProb7)
  allTailRecur(isEven _, listDToTestForProb7)

  allMapFilterReduce(isPal _, listAToTestForProb7)
  allMapFilterReduce(isPal _, listBToTestForProb7)
  allMapFilterReduce(isEven _, listCToTestForProb7)
  allMapFilterReduce(isEven _, listDToTestForProb7)

  //**************

  // problem 8
  // iterative version

  // recursive version

  // tail-recursive version

  // map-filter-reduce version
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

//  primes(3)
  // end of lecture example
  // 3. The stream of all non-negative even integers
  // 4. The stream of all squares of integers
  // tests:
  //**************


}