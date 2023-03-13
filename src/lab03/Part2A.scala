package lab03

object Part2A extends App:
  /* esercizio 1: higher-order function. Utilizzare sia funzione lambda match-case, sia metodo*/

  def checkPositive(n: Int): String = n match
    case n if n >=0 => "positive"
    case _ => "negative"


  val m = 5
  val n = -3

  println( m + " is a " + checkPositive(m) + " number")
  println( n + " is a " + checkPositive(n) + " number")

  val lambdaCheckPositive: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println( " 3 is a " + lambdaCheckPositive(3) + " number")

  /*esercizio 2: neg function che accetta un predicato su stringa e restituisce un altro predicato, l'opposto*/
  val biggerThanTen: String => Boolean = _.size > 10

  def neg(predicate: String => Boolean): String => Boolean = s => !predicate(s)
  val lambdaNeg: (String => Boolean) => (String => Boolean) = f => y => !f(y)

  val saluto = "ciao a tutti ragazzi e ragazze"
  val arrivederci = "ciao ciao"
  val smallerThanTen = neg(biggerThanTen)
  val smallerLambda = lambdaNeg(biggerThanTen)
  println(biggerThanTen(saluto))
  println(biggerThanTen(arrivederci))
  println(smallerThanTen(arrivederci))
  println(smallerLambda(arrivederci))
  println(smallerLambda(saluto))

  def genericNeg[X](predicate: X => Boolean): X => Boolean = x => !predicate(x)
  val genericLambdaNeg: (Any => Boolean) => (Any => Boolean) = f => y => !f(y)

  val negative: Int => Boolean = _ < 0
  val positive = genericNeg(negative)
  //val lambdaPositive = genericLambdaNeg(negative)
  println("genericNeg test")
  println(negative(m))
  println(positive(m))
  //println(lambdaPositive(m))
  //println(lambdaPositive(n))
