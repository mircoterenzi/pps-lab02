object Tasks extends App :

  // Task 1, Svolto da solo
  val hello: String = "Hello, Scala"
  println(hello)

  def mult(x: Double, y: Double): Double = x * y
  def multCurried(x: Double)(y: Double): Double = x * y
  println(s"mult(2,3) = ${mult(2,3)}; multCurried(2)(3) = ${multCurried(2)(3)}; multCurried(2) = ${multCurried(2)}")

  val f: (Int => Int) => Int => Int = f => i => f(i)
  println(s"f(_ + 1)(2) = ${f(_ + 1)(2)}; equals to f(x => x + 1)(2) = ${f(x => x + 1)(2)}")

  val f1: (Int => Int) => Int = _(3)
  val f2 = f1(_)
  println(s"f1(_ + 2) = ${f1(_ + 2)}; equals to f2(v => v + 2) = ${f1(v => v + 2)}")
  println(s"f1(_) equals to f1(): f1(_ + 2) = ${f1(_ + 2)}; f2(_ + 2) = ${f2(_ + 2)}")

  val multBy3 = multCurried(3)
  println(s"mult(3,2) = ${mult(3,2)}; multBy3(2) = ${multBy3(2)}")

  val p: (Int, Int, Int, Int) => Int = _ + _ + _ + _
  val sqrP = (a: Int, b: Int) => p(a, a, b, b)

  def div: (Double, Double) => Double = _ / _
  def divCurried(x: Double)(y: Double): Double = x / y
  val div6By = divCurried(6)
  println(s"div(6,3) = ${div(6,3)}; divCurried(6)(3) = ${divCurried(6)(3)}; div6by(3) = ${div6By(3)}")

  def f3(f: Double => Double)(a: Double): Double = f(a)
  val multBy2 = f3(_ * 2)
  println("multBy2(3) = " + multBy2(3))

  def divCurried2(a: Double)(b: Double): Double = f3(a / _)(b)
  println("divCurried2(8)(4) = " + divCurried2(8)(4))

  // Task 2, Svolto da solo
  val posLambda: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"
  def posMethod: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"

  // Type neg = (String => Boolean) => String => Boolean
  val negLambda: (String => Boolean) => String => Boolean = f => x => !f(x)
  def negMethod(f: String => Boolean): String => Boolean = x => !f(x)
  val empty: String => Boolean = _ == ""
  // type notEmpty = String => Boolean
  val notEmpty = negLambda(empty)
  println(s"notEmpty(\"foo\") = ${notEmpty("foo")}; " +
    s"notEmpty(\"\") = ${notEmpty("")}; " +
    s"notEmpty(\"foo\") && !notEmpty(\"\") ${notEmpty("foo") && !notEmpty("")}"
  )

  def neg[X]: (X => Boolean) => X => Boolean = f => x => !f(x)
  val leZero: Int => Boolean = _ <= 0
  val dog: String => Boolean = _ == "dog"
  val gZero = neg(leZero)
  val notDog = neg(dog)
  println(s"gZero(1) = ${gZero(1)}; notDog(\"cat\") = ${notDog("cat")}")

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2 = (x: Int, y: Int, z: Int) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  def p4(x: Int, y: Int, z: Int) = x <= y && y == z
  println(s"input = (3,4,4): p1 = ${p1(3)(4)(4)}; p2 = ${p2(3, 4, 4)}; p3 = ${p3(3)(4)(4)}; p4 = ${p4(3, 4, 4)}")
  println(s"input = (3,4,5): p1 = ${p1(3)(4)(5)}; p2 = ${p2(3, 4, 5)}; p3 = ${p3(3)(4)(5)}; p4 = ${p4(3, 4, 5)}")

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
  def composeGen[A,B,C] (f: B => C, g: A => B): A  => C = x => f(g(x))
  // Signature composeGen = (f: B => C, g: A => B): A  => C
  println(s"compose(_ - 1, _ * 2)(5) = ${compose(_ - 1, _ * 2)(5)}; " +
    s"composeGen[Int](_ - 1, _ * 2)(5) = ${composeGen[Int, Int, Int](_ - 1, _ * 2)(5)}"
  )
  println("composeGen(_ concat \"Scala\", _ concat \",\")(\"Hello\") = " +
    composeGen[String, String, String](_ concat "Scala", _ concat ", ")("Hello")
  )

  def composeThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = x => f(g(h(x)))
  def composeForThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = composeGen(f, composeGen(g, h))
  println("composeThree(_ + \"!\", _.toString, _ * 2)(3) = " +
    composeThree[Int, Int, String, String](_ + "!", _.toString, _ * 2)(3) + "; " +
    "composeForThree(_ + \"!\", _.toString, _ * 2)(3) = " +
    composeForThree[Int, Int, String, String](_ + "!", _.toString, _ * 2)(3)
  )

  // Task 3
  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case _ => base * power(base, exponent - 1)
  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(exponent: Int, tot: Double): Double = exponent match
      case 0 => tot
      case _ => _power(exponent - 1, base * tot)
    _power(exponent, 1)
  println(s"power(2, 3) = ${power(2, 3)}; power(5, 2) = ${power(5, 2)}")
  println(s"powerTail(2, 3) = ${powerTail(2, 3)}; powerTail(5, 2) = ${powerTail(5, 2)}")

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(n: Int, res: Int): Int = n match
      case 0 => res
      case _ => _reverse(n / 10, (res * 10) + (n % 10))
    _reverse(n, 0)
  println("reverseNumber(12345) = " + reverseNumber(12345))
