sealed trait Stmt
case class Assign(loc: Int, lvalue: LValue, exp: Exp)
case class StmtExp(loc: Int, exp: Exp)
case class StmtLabel(loc: Int, lab: Int)
case class Jump(loc: Int, lab: Int)

sealed trait Exp
case class CallMethod(cls: ClassDesc, method: String, args: List[Exp]) extends Exp
case class Add(e1: Exp, e2: Exp) extends Exp
case class Sub(e1: Exp, e2: Exp) extends Exp
case class ExpValue(values: List[Value]) extends Exp

sealed trait Value
case class Unknown() extends Value
case class StringLiteral(s: String) extends Value
case class IntLiteral(n: Int) extends Value

sealed trait ClassDesc
case class ClassName(name: String) extends ClassDesc

sealed trait LValue
case class LValueVar(ident: String) extends LValue

class Rules(context: Context) {
  def rules(exp: Exp): List[String]  = exp match {

    case CallMethod(ClassName("javax.crypto.Cipher"), "getInstance", List(ExpValue(arg1)))
        if arg1.contains(Unknown) =>
      List("maybe unsafe")

    case CallMethod(ClassName("javax.crypto.Cipher"), "getInstance", List(ExpValue(arg1)))
        if arg1.contains(StringLiteral("DES/ECB/NoPadding")) =>
      List("maybe unsafe")

    case CallMethod(cls, method, args)
        if context.deprecatedMethod(cls, method) =>
      List("call to deprecated method: ${cls.name}.${method}")

    case _ => List()
  }
}

trait Context {
  def deprecatedMethod(cls: ClassDesc, method: String): Boolean = deprecated.contains( (cls, method) )

  val deprecated: Set[(ClassDesc, String)] = Set((ClassName("java.core"), "useRust"))
}
