import Lab2.Expr
import org.junit.*
import org.junit.Assert.*

class ExprTest:
  @Test def testCreateLiteral(): Unit = {
    val literal = Expr.Literal(2)
    assertNotNull(literal)
  }