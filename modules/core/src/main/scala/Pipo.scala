package pipo.core

sealed trait PipoExpr
case class PipoGrammar(fields: List[PipoField]) extends PipoExpr
case class PipoField(namespace: PipoNamespace) extends PipoExpr
case class PipoNamespace(name: PipoIdentifier, variables: List[PipoVariable]) extends PipoExpr
case class PipoVariable(key: PipoKey, value: List[PipoValue]) extends PipoExpr
case class PipoKey(get: PipoIdentifier) extends PipoExpr
case class PipoValue(get: Either[PipoIdentifier, PipoInterpolate]) extends PipoExpr
case class PipoInterpolate(property: PipoProperty) extends PipoExpr
case class PipoProperty(keys: List[PipoKey]) extends PipoExpr
case class PipoIdentifier(get: String) extends PipoExpr

object Pipo {
  private def pipoParser: Parser[PipoExpr] = Parser { s =>
    def pGrammar: Parser[PipoExpr] =
      ???

    pGrammar.run(s)
  }
}
