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
  /*esercizio 3: currying*/
  val x = 5
  val y = 6
  val z = 6

  val curriedFunction: Double => Double => (Double => Boolean) = x => y => (z => x <= y && y == z)

  println("result of curried function: " + curriedFunction(x)(y)(z))

  val nonCurriedFunction: (Double, Double, Double) => Boolean = (x, y, z) => x <= y && y == z

  println("result of non curried function: " + nonCurriedFunction(x, y, z))

  def curriredDef(x: Int)(y: Int)(z: Int): Boolean = (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  println("result of curried def: " + curriredDef(x)(y)(z))

  def nonCurriedDef(x: Int, y: Int, z: Int): Boolean = (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  println("result of non curried def: " + nonCurriedDef(x, y, z))

  /*esercizio 4: crea una funzione che funge da composizione di due altre funzioni*/
  def compose(f: Int => Int)(g: Int => Int): Int => Int = x => f(g(x))

  println("raddoppiando 6 e aggiungendo 1 ottengo come risultato " + compose(_ + 1)(_ * 2)(6))

  val lambdaCompose: (Int => Int, Int => Int) => (Int => Int) = (f, g) => (n => f(g(n)))

  println("dividendo per 2 il numero 10 e moltiplicando per 3 ottengo come risultato " + lambdaCompose(_ / 2, _ * 3)(10))
