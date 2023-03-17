package lab03

object Recursion extends App:
  
  def gcd(a: Int, b: Int):Int = (a, b) match
    case (a, 0) => a
    case (0, b) => b
    case (a, b) if a > b => gcd(b, a % b)
    case _ => gcd(a, b % a)

  println(gcd(12, 8))
  println(gcd(14, 7))
  println(gcd(2, 9))

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

  println(perimeter(Shape.Rectangle(2, 4)))
  println(perimeter(Shape.Circle(3.5)))
  println(perimeter(Shape.Square(4.2)))

  println(contains(Shape.Rectangle(2, 4), (1.2, 3.5)))
  println(contains(Shape.Rectangle(2, 4), (3.5, 4.2)))
  println(contains(Shape.Square(5.9), (3.4, 5)))
  println(contains(Shape.Square(8.9), (2.3, 9)))
