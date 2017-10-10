object ListProcessing2 {
  // problem 1, 2, 3, 4

  // problem 1
  // avg of scores
  def avg(scores: List[Double]): Double = {
    if (scores.length == 0) throw new Exception("there is no scores")
    var sum = 0.0
    for (score <- scores) sum += score
    sum / scores.length
  }

  // list of averages for each student
  def avgAvg(scores: List[List[Double]]): List[Double] = scores.map(avg _)

  // list of positions in the list with avg >= 70
  def passing(scores: List[List[Double]]): List[Int] = {
    val avgScores = scores.map(avg _)
    var position = 0
    def helper(result: List[Int], count: Int, unseenScores: List[Double]) : List[Int] = {
      if (unseenScores == Nil) result
      else if (unseenScores.head > 70) helper(result :+ count, count + 1, unseenScores.tail)
      else helper(result, count + 1, unseenScores.tail)
    }
    helper(Nil, 0, avgScores)
  }

  // sum of sums of all scores
  def sum(scores: List[Double]): Double = {
    if (scores.length == 0) throw new Exception("there is no scores")
    var sum = 0.0
    for (score <- scores) sum += score
    sum
  }
  def sumSums(scores: List[List[Double]]):Double = scores.map(sum _).sum
  // tests:
  val test1 = List(95.0, 100.0, 87.0, 97.0)
  avg(test1)
  sum(test1)
  val cs152 = List(List(93.0, 89.0, 90.0), List(75.0, 76.0, 68.0), List(68.0, 62.0, 58.0))
  avgAvg(cs152)
  passing(cs152)
  sumSums(cs152)
  //********************

  // problem 2
  // all words in doc not in dictionary
  def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {
    def helper(result: List[String], unseenDoc: List[String]) : List[String] = {
      if (unseenDoc == Nil) result
      else if (!dictionary.contains(unseenDoc.head)) helper(result:+unseenDoc.head, unseenDoc.tail)
      else helper(result, unseenDoc.tail)
    }
    helper(Nil, doc)
  }
  // tests:
  val dict1 = List("hello", "world", "is", "not", "round")
  val doc1 = List("hello", "world", "is", "like", "an", "oval")
  val notItDict1 = spellCheck(doc1, dict1)
  //********************

  // problem 3
  // all words in doc not in dictionary without using recursion or iteration but
  // with map, filter, reduce

  def spellCheckMapFilterReduce(doc: List[String], dictionary: List[String]): List[String] = {
    def notInDictionary(s: String) = !dictionary.contains(s)
    doc.filter(notInDictionary _)
  }
  // tests:
  val dict2 = List("hello", "world", "is", "not", "round")
  val doc2 = List("hello", "world", "is", "like", "an", "oval")
  val notItDict2 = spellCheckMapFilterReduce(doc2, dict2)
  //********************

  // problem 4
  // result of substituting x in mono
  def evalMono(mono: (Double, Double), x: Double): Double = mono._1 * math.pow(x, mono._2)

  // result of substituting x in poly
  def evalPoly(poly: List[(Double, Double)], x: Double): Double = {
    var result = 0.0
    for (p <- poly)
      result += evalMono(p, x)
    result
  }
  // tests:
  val poly1 = List((3.0, 2.0), (-5.0, 0.0))
  val poly2 = List((4.0, 5.0), (-5.0, 0.0), (7.0, 2.0))
  evalPoly(poly1, 2)
  evalPoly(poly2, 5)
}