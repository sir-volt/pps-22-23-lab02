package lab03

object Tasks extends App:
  /* esercizio 1: higher-order function. Utilizzare sia funzione lambda match-case, sia metodo*/

  def checkPositive(n: Int): String = n match
    case n if n >=0 => "positive"
    case _ => "negative"


  val m = 5
  val n = -3

  //println( m + " is a " + checkPositive(m) + " number")
  //println( n + " is a " + checkPositive(n) + " number")

  val lambdaCheckPositive: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  //println( " 3 is a " + lambdaCheckPositive(3) + " number")

  /*esercizio 2: neg function che accetta un predicato su stringa e restituisce un altro predicato, l'opposto*/
  val biggerThanTen: String => Boolean = _.size > 10

  def neg(predicate: String => Boolean): String => Boolean = s => !predicate(s)
  val lambdaNeg: (String => Boolean) => (String => Boolean) = f => y => !f(y)

  val saluto = "ciao a tutti ragazzi e ragazze"
  val arrivederci = "ciao ciao"
  val smallerThanTen = neg(biggerThanTen)
  val smallerLambda = lambdaNeg(biggerThanTen)
  //println(biggerThanTen(saluto))
  //println(biggerThanTen(arrivederci))
  //println(smallerThanTen(arrivederci))
  //println(smallerLambda(arrivederci))
  //println(smallerLambda(saluto))

  def genericNeg[X](predicate: X => Boolean): X => Boolean = x => !predicate(x)
  val genericLambdaNeg: (Any => Boolean) => (Any => Boolean) = f => y => !f(y)

  val negative: Int => Boolean = _ < 0
  val positive = genericNeg(negative)
  //println("genericNeg test")
  //println(negative(m))
  //println(positive(m))
  //println(lambdaPositive(m))
  //println(lambdaPositive(n))
  /*esercizio 3: currying*/
  val x = 5
  val y = 6
  val z = 6

  val curriedFunction: Double => Double => (Double => Boolean) = x => y => (z => x <= y && y == z)

  //println("result of curried function: " + curriedFunction(x)(y)(z))

  val nonCurriedFunction: (Double, Double, Double) => Boolean = (x, y, z) => x <= y && y == z

  //println("result of non curried function: " + nonCurriedFunction(x, y, z))

  def curriredDef(x: Int)(y: Int)(z: Int): Boolean = (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  //println("result of curried def: " + curriredDef(x)(y)(z))

  def nonCurriedDef(x: Int, y: Int, z: Int): Boolean = (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  //println("result of non curried def: " + nonCurriedDef(x, y, z))

  /*esercizio 4: crea una funzione che funge da composizione di due altre funzioni*/
  def compose(f: Int => Int)(g: Int => Int): Int => Int = x => f(g(x))

  //println("raddoppiando 6 e aggiungendo 1 ottengo come risultato " + compose(_ + 1)(_ * 2)(6))

  val lambdaCompose: (Int => Int, Int => Int) => (Int => Int) = (f, g) => (n => f(g(n)))

  //println("dividendo per 2 il numero 10 e moltiplicando per 3 ottengo come risultato " + lambdaCompose(_ / 2, _ * 3)(10))

  /*esercizio 5: creare una funzione che esegue il greatest commond divisor (GCD) di due integer tramite ricorsione*/
  def gcd(a: Int, b: Int): Int = (a, b) match
    case (a, 0) => a
    case (0, b) => b
    case (a, b) if a > b => gcd(b, a % b)
    case _ => gcd(a, b % a)

  //println(gcd(12, 8))
  //println(gcd(14, 7))
  //println(gcd(2, 9))

  /*esercizio 6: creare un Enum Shape contenente Rectangle, Circle e Square; sviluppare a seguito le funzioni "perimeter"
  per trovare il perimetro e "contains", per scoprire se dato un punto, questo faccia parte della figura passata in ingresso
  al metodo*/
  enum Shape:
    case Rectangle(l1: Double, l2: Double)
    case Circle(r: Double)
    case Square(l: Double)

  def perimeter(shape: Shape): Double = shape match
    case Shape.Rectangle(l1, l2) => (l1 + l2) * 2
    case Shape.Circle(r) => r * 2 * Math.PI
    case Shape.Square(l) => l * 4

  //rectangle and square both start from point (0, 0)
  //circle has its center at (r, r)
  def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
    case Shape.Rectangle(l1, l2) => point._1 <= l1 && point._2 <= l2
    case Shape.Square(l) => point._1 <= l && point._2 <= l
    case Shape.Circle(r) => point._1 <= 2 * r && point._2 <= 2 * r

  /*
  println(perimeter(Shape.Rectangle(2, 4)))
  println(perimeter(Shape.Circle(3.5)))
  println(perimeter(Shape.Square(4.2)))

  println(contains(Shape.Rectangle(2, 4), (1.2, 3.5)))
  println(contains(Shape.Rectangle(2, 4), (3.5, 4.2)))
  println(contains(Shape.Square(5.9), (3.4, 5)))
  println(contains(Shape.Square(8.9), (2.3, 9)))*/

  /*ultimo esercizio: espandere Option sviluppando i metodi filter(per filtrare determinati Option se soddisfano una
  condizione passata in ingresso al metodo), map(per trasformare il valore contenuto in un Option se esiste) e
  fold(che restituisce un valore default se Option non contiene valore o un nuovo valore derivato dall'applicazione
  di una funzione lambda passata in ingresso sull'elemento contenuto dentro l'Option)*/
  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:

    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(f: A => Boolean): Option[A] = opt match
      case Some(a) if f(a) == true => Some(a)
      case _ => None()

    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt match
      case Some(a) => Some(f(a))
      case _ => None()

    def fold[A, B](opt: Option[A])(default: B)(f: A => B): B = opt match
      case Some(a) => f(a)
      case _ => default

  import Option.*

  val s1: Option[Int] = Some(1)
  val s2: Option[Int] = Some(2)
  val s3: Option[Int] = None()
  val s4: Option[Int] = Some(5)

  /*
  println(s1) // Some(1)
  println(orElse(s1, 0))
  println(orElse(s3, 0)) // 1,0
  println(flatMap(s1)(i => Some(i + 1))) // Some(2)
  println(flatMap(s1)(i => flatMap(s2)(j => Some(i + j)))) // Some(3)
  println(flatMap(s1)(i => flatMap(s3)(j => Some(i + j)))) // None
  println(filter(s2)(_ > 0))
  println(filter(s1)(_ > 3))
  println(filter(s3)(_ == 5))
  println(map(s4)(_ > 3))
  println(map(s4)(_ > 8))
  println(map(s3)(_ > 2))
  println(fold(s4)(1)(_ + 1))
  println(fold(s3)(2)(_ * 5))*/
