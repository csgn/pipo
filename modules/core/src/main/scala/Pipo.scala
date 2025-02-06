package pipo.core

import Parser._
import Symbol._

sealed trait PipoExpr
case class PipoGrammar(fields: List[PipoField]) extends PipoExpr
case class PipoField(namespace: PipoNamespace) extends PipoExpr
case class PipoNamespace(name: PipoLit, variables: List[PipoVariable]) extends PipoExpr
case class PipoVariable(key: PipoKey, value: PipoValue) extends PipoExpr
case class PipoKey(get: PipoLit) extends PipoExpr
case class PipoValue(get: PipoLit) extends PipoExpr
case class PipoLit(get: String) extends PipoExpr

object Pipo {
  def pLit(p: Parser[String]): Parser[PipoLit] =
    p.map(PipoLit)

  def pValue: Parser[PipoValue] =
    pLit(until(Newline))
      .map(PipoValue)

  def pKey: Parser[PipoKey] =
    pLit(until(Equal, OpenBracket, Newline, Whitespace))
      .map(PipoKey)

  def pVariable: Parser[PipoVariable] =
    // pKey must therfore we don't need to evaluate the right side.
    (pKey <* ws <* char(Equal) !~ ws *> pValue <* ws <* nl)
      .map {
        // TODO should be error
        case (None, None) => {
          PipoVariable(PipoKey(PipoLit("NaN")), PipoValue(PipoLit("NaN")))
        }
        // TODO should be error
        case (None, Some(value)) => {
          PipoVariable(PipoKey(PipoLit("NaN")), PipoValue(PipoLit("NaN")))
        }
        case (Some(key), None)        => PipoVariable(key, PipoValue(PipoLit("NaN")))
        case (Some(key), Some(value)) => PipoVariable(key, value)
      }

  def pNamespace: Parser[PipoNamespace] =
    (char(OpenBracket) *> pLit(until(CloseBracket)) <* char(CloseBracket) !~ nl *> pVariable.many)
      .map {
        // TODO should be error
        case (None, None) => {
          PipoNamespace(PipoLit("NaN"), Nil)
        }
        // TODO should be error
        case (None, Some(a)) => {
          PipoNamespace(PipoLit("NaN"), Nil)
        }
        case (Some(name), None)            => PipoNamespace(name, Nil)
        case (Some(name), Some(variables)) => PipoNamespace(name, variables)
      }

  def pField: Parser[PipoField] =
    nl *> pNamespace.map(PipoField) <* nl

  def pGrammar: Parser[PipoGrammar] =
    pField.many.map(PipoGrammar)

  def apply(): Parser[PipoExpr] = Parser { s =>
    pGrammar.run(s)
  }
}
