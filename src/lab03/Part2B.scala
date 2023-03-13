package lab03

object Part2B extends App:

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

  //val lambdaCompose: (Int => Int) => ((Int => Int) => (Int => Int)) => f => (g => f(g))


