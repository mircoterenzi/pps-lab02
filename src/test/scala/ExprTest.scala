import Lab2.Expr
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
