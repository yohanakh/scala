
/**
  * Created by Barak Bar Orion
  * on 7/20/16.
  *
  * @since 12.0
  */
object Main {
  def main(args: Array[String]): Unit = {

    //    val e = BinOp("+", Number(1), BinOp("+", Number(2), Number(1)))
//    var e = BinOp("+", BinOp("+", Number(1), Number(0)), BinOp("+", Number(0), Number(1)))
//    e = BinOp("*", e, Number(1))
//    val res = simplify(e)
//
//    println(res)

    val res1 = simplify(UnOp("-", UnOp("-", Number(1))))
    val res2 = simplify(UnOp("-", UnOp("-", UnOp("-", Number(1)))))
    val res3 = simplify(UnOp("-", UnOp("-", UnOp("-", UnOp("-", Number(1))))))
    val res4 = simplify(BinOp("+", Number(0), BinOp("+", Number(0), Number(1))))
    val res5 = simplify(BinOp("*", UnOp("-", UnOp("-", Number(1))), BinOp("+", Number(2), Number(2))))
    val res6 = simplify(BinOp("+", BinOp("​*", Number(5), Number(5)), BinOp("*​", Number(5), Number(5))))

    println(s"res1: $res1")
    println(s"res2: $res2")
    println(s"res3: $res3")
    println(s"res4: $res4")
    println(s"res5: $res5")
    println(s"res6: $res6")

    println()

  }

  def simplify(expr: Expr): Expr = {
    expr match {
      case UnOp("-", UnOp("-", e)) => simplify(e)
      // Double negation
      case BinOp("+", e, Number(0)) => simplify(e) // Adding zero
      case BinOp("+", Number(0), e) => simplify(e) // Adding zero
      case BinOp("*", e, Number(1)) => simplify(e)
      case BinOp("*", Number(1), e) => simplify(e)
      case UnOp(x, e) => UnOp(x, simplify(e))
//      case a@BinOp(x, Var(_)|Number(_), Var(_)|Number(_)) => a
      case e@BinOp(x, left, right) => {
        val newleft = simplify(left)
        val newright = simplify(right)

        if (newleft == left && newright == right) {
          expr
        } else {
          simplify(BinOp(x, newleft, newright))
        }
      }
      case _ => expr // Multiplying by one
    }
  }

}
