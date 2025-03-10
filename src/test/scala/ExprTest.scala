import Lab2.{Expr, evaluate}
import org.junit.*
import org.junit.Assert.*

class ExprTest:
  @Test def testCreateLiteral(): Unit = {
    assertNotNull(Expr.Literal(2))
  }

  @Test def testCreateAddExpr(): Unit = {
    val literal = Expr.Literal(2)
    assertNotNull(Expr.Add(literal, literal))
  }

  @Test def testCreateMultiplyExpr(): Unit = {
    val literal = Expr.Literal(2)
    assertNotNull(Expr.Multiply(literal, literal))
  }

  @Test def testCreateComplexExpression(): Unit = {
    val literal = Expr.Literal(2)
    assertNotNull(Expr.Multiply(Expr.Add(literal, literal), Expr.Multiply(literal, literal)))
  }

  @Test def testEvaluateLiteralReturnValue(): Unit = {
    assertEquals(2, evaluate(Expr.Literal(2)))
  }

  @Test def testEvaluateAddReturnSumOfValues(): Unit = {
    val firstValue = Expr.Literal(2)
    val secondValue = Expr.Literal(5)
    assertEquals(evaluate(firstValue) + evaluate(secondValue), evaluate(Expr.Add(firstValue, secondValue)))
  }

  @Test def testEvaluateMultiplyReturnTimesValues(): Unit = {
    val firstValue = Expr.Literal(2)
    val secondValue = Expr.Literal(5)
    assertEquals(evaluate(firstValue) * evaluate(secondValue), evaluate(Expr.Multiply(firstValue, secondValue)))
  }
