import Lab2.Expr
import org.junit.*
import org.junit.Assert.*

class ExprTest:
  @Test def testCreateLiteral(): Unit = {
    val literal = Expr.Literal(2)
    assertNotNull(literal)
  }

  @Test def testCreateAddExpr(): Unit = {
    val literal = Expr.Literal(2)
    assertNotNull(Expr.Add(literal, literal))
  }
