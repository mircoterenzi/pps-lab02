import Lab2.{Expr, evaluate, show}
import org.junit.*
import org.junit.Assert.*

class ExprTest:
  @Test def testCreateLiteral(): Unit =
    assertNotNull(Expr.Literal(2))

  @Test def testCreateAddExpr(): Unit =
    val literal = Expr.Literal(2)
    assertNotNull(Expr.Add(literal, literal))

  @Test def testCreateMultiplyExpr(): Unit =
    val literal = Expr.Literal(2)
    assertNotNull(Expr.Multiply(literal, literal))

  @Test def testCreateComplexExpression(): Unit =
    val literal = Expr.Literal(2)
    assertNotNull(Expr.Multiply(Expr.Add(literal, literal), Expr.Multiply(literal, literal)))

  @Test def testEvaluateLiteralReturnValue(): Unit =
    assertEquals(2, evaluate(Expr.Literal(2)))

  @Test def testEvaluateAddReturnSumOfValues(): Unit =
    val firstValue = Expr.Literal(2)
    val secondValue = Expr.Literal(5)
    assertEquals(2 + 5, evaluate(Expr.Add(firstValue, secondValue)))

  @Test def testEvaluateMultiplyReturnTimesValues(): Unit =
    val firstValue = Expr.Literal(4)
    val secondValue = Expr.Literal(5)
    assertEquals(4 * 5, evaluate(Expr.Multiply(firstValue, secondValue)))

  @Test def testShowLiteral(): Unit =
    assertEquals("10", show(Expr.Literal(10)))

  @Test def testShowAdd(): Unit =
    val firstValue = Expr.Literal(4)
    val secondValue = Expr.Literal(5)
    assertEquals("(4+5)", show(Expr.Add(firstValue, secondValue)))

  @Test def testShowMultiply(): Unit =
    val firstValue = Expr.Literal(4)
    val secondValue = Expr.Literal(5)
    assertEquals("(4*5)", show(Expr.Multiply(firstValue, secondValue)))

  @Test def testComputeComplexExpression(): Unit =
    val expr = Expr.Add(
      Expr.Multiply(
        Expr.Multiply(
          Expr.Literal(4),
          Expr.Literal(5)
        ),
        Expr.Literal(5)
      ),
      Expr.Literal(2)
    )
    assertEquals("(((4*5)*5)+2)", show(expr))
    assertEquals(102, evaluate(expr))
