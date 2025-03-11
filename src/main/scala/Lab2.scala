object Lab2 extends App :

  def printFormatted[X](expected: X, actual: X) = println(s"expected: $expected - got: $actual")

  // Task 1, Svolto con Giangiulli Chiara e Shtini Dilaver (prima consegna)
  val hello: String = "Hello, Scala"
  println(hello)

  def mult(x: Double, y: Double): Double = x * y
  def multCurried(x: Double)(y: Double): Double = x * y
  printFormatted(mult(2,3), multCurried(2)(3))

  val f: (Int => Int) => Int => Int = f => i => f(i)
  printFormatted(f(x => x + 1)(2), f(_ + 1)(2))

  val f1: (Int => Int) => Int = _(3)
  val f2 = f1(_)
  printFormatted(f1(v => v + 2), f1(_ + 2))
  printFormatted(f1(_ + 2), f2(_ + 2))

  val multBy3 = multCurried(3)
  printFormatted(mult(3,2), multBy3(2))

  val p: (Int, Int, Int, Int) => Int = _ + _ + _ + _
  val sqrP = (a: Int, b: Int) => p(a, a, b, b)

  def div: (Double, Double) => Double = _ / _
  def divCurried(x: Double)(y: Double): Double = x / y
  val div6By = divCurried(6)
  printFormatted(div(6,3), divCurried(6)(3))
  printFormatted(div(6,3), div6By(3))

  def f3(f: Double => Double)(a: Double): Double = f(a)
  val multBy2 = f3(_ * 2)
  printFormatted(6.0, multBy2(3))

  def divCurried2(a: Double)(b: Double): Double = f3(a / _)(b)
  printFormatted(2.0, divCurried2(8)(4))

  // Task 2, Svolto da solo
  val pos1: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"
  def pos2: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"

  // Type neg = (String => Boolean) => String => Boolean
  val neg1: (String => Boolean) => String => Boolean = f => x => !f(x)
  def neg2(f: String => Boolean): String => Boolean = x => !f(x)
  val empty: String => Boolean = _ == ""
  // type notEmpty = String => Boolean
  val notEmpty = neg1(empty)
  printFormatted(true, notEmpty("foo"))
  printFormatted(false, notEmpty(""))
  printFormatted(true, notEmpty("foo") && !notEmpty(""))

  def neg[X]: (X => Boolean) => X => Boolean = f => x => !f(x)
  val leZero: Int => Boolean = _ <= 0
  val dog: String => Boolean = _ == "dog"
  val gZero = neg(leZero)
  val notDog = neg(dog)
  printFormatted(true, gZero(1))
  printFormatted(true, notDog("cat"))

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2 = (x: Int, y: Int, z: Int) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  def p4(x: Int, y: Int, z: Int) = x <= y && y == z
  println("expected: true - got:" +
    s"\n\tp1 = ${p1(3)(4)(4)}" +
    s"\n\tp2 = ${p2(3, 4, 4)}" +
    s"\n\tp3 = ${p3(3)(4)(4)}" +
    s"\n\tp4 = ${p4(3, 4, 4)}")
  println("expected: false - got:" +
    s"\n\tp1 = ${p1(3)(4)(5)}" +
    s"\n\tp2 = ${p2(3, 4, 5)}" +
    s"\n\tp3 = ${p3(3)(4)(5)}" +
    s"\n\tp4 = ${p4(3, 4, 5)}")

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  def composeGen[A,B,C] (f: B => C, g: A => B): A  => C = x => f(g(x))
  // Signature composeGen = (f: B => C, g: A => B): A  => C
  printFormatted(compose(_ - 1, _ * 2)(5), composeGen[Int, Int, Int](_ - 1, _ * 2)(5))
  printFormatted("Hello, Scala", composeGen[String, String, String](_ + "Scala", _ + ", ")("Hello"))

  def composeThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = x => f(g(h(x)))
  def composeThree2[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = composeGen(f, composeGen(g, h))
  printFormatted(
    composeThree[Int, Int, String, String](_ + "!", _.toString, _ * 2)(3),
    composeThree2[Int, Int, String, String](_ + "!", _.toString, _ * 2)(3)
  )

  // Task 3, Svolto da solo
  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case _ => base * power(base, exponent - 1)
  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(exponent: Int, tot: Double): Double = exponent match
      case 0 => tot
      case _ => _power(exponent - 1, base * tot)
    _power(exponent, 1)
  printFormatted(power(2, 3), powerTail(2, 3))
  printFormatted(power(5, 2), powerTail(5, 2))

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(n: Int, res: Int): Int = n match
      case 0 => res
      case _ => _reverse(n / 10, (res * 10) + (n % 10))
    _reverse(n, 0)
  printFormatted(54321, reverseNumber(12345))

  // Task 4, Svolto da solo
  // Test: https://github.com/mircoterenzi/pps-lab02/blob/main/src/test/scala/ExprTest.scala
  enum Expr:
    case Literal(value: Int)
    case Add(first: Expr, second: Expr)
    case Multiply(first: Expr, second: Expr)

  def evaluate(expr: Expr): Int = expr match
    case Expr.Literal(v) => v
    case Expr.Add(f, s) => evaluate(f) + evaluate(s)
    case Expr.Multiply(f, s) => evaluate(f) * evaluate(s)

  def show(expr: Expr): String = expr match
    case Expr.Literal(v) => s"$v"
    case Expr.Add(f, s) => "(" + show(f) + "+" + show(s) + ")"
    case Expr.Multiply(f, s) => "(" + show(f)  + "*" + show(s) + ")"

  // Task 5, Svolto da solo
  // Test: https://github.com/mircoterenzi/pps-lab02/blob/main/src/test/scala/task5/OptionalTest.scala
  // Class: https://github.com/mircoterenzi/pps-lab02/blob/main/src/main/scala/task5/Optional.scala