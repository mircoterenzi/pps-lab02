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
