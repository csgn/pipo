package pipo.core

import Parser._

sealed trait PipoExpr
case class PipoGrammar(fields: List[PipoField]) extends PipoExpr
case class PipoField(namespace: PipoNamespace) extends PipoExpr
case class PipoNamespace(name: PipoIdentifier, variables: List[PipoVariable]) extends PipoExpr
case class PipoVariable(key: PipoKey, value: List[PipoValue]) extends PipoExpr
case class PipoKey(get: PipoIdentifier) extends PipoExpr
case class PipoValue(get: Either[PipoInterpolate, PipoIdentifier]) extends PipoExpr
case class PipoInterpolate(property: PipoProperty) extends PipoExpr
case class PipoProperty(keys: List[PipoIdentifier]) extends PipoExpr
case class PipoIdentifier(get: String) extends PipoExpr

object Pipo {
  private def pipoParser: Parser[PipoExpr] = Parser { s =>

    def pIdentifier(p: Parser[String]): Parser[PipoIdentifier] =
      p.map(PipoIdentifier)

    def pProperty: Parser[PipoProperty] =
      pIdentifier(span(_ != '}')).many
        .map(PipoProperty)

    def pInterpolate: Parser[PipoInterpolate] =
      (string("{{") *> pProperty <* string("}}"))
        .map(PipoInterpolate)

    def pValue: Parser[PipoValue] =
      (pInterpolate <||> pIdentifier(span(_ != '\n')))
        .map {
          case left @ Left(_)   => PipoValue(left)
          case right @ Right(_) => PipoValue(right)
        }

    def pKey: Parser[PipoKey] =
      pIdentifier(span(_ != '=').debug)
        .map(PipoKey)

    def pVariable: Parser[PipoVariable] =
      // pKey must we don't need to evaluate the right side.
      (pKey !~ (char('=') *> pValue.many <* char('\n')))
        .map {
          case (None, None) => {
            PipoVariable(PipoKey(PipoIdentifier("NaN")), Nil)
          }
          case (None, Some(value)) => {
            PipoVariable(PipoKey(PipoIdentifier("NaN")), Nil)
          }
          case (Some(key), None)        => PipoVariable(key, Nil)
          case (Some(key), Some(value)) => PipoVariable(key, value)
        }

    def pNamespace: Parser[PipoNamespace] =
      ((char('[') *> pIdentifier(span(_ != ']')) <* char(']')) !~ (char('\n') *> pVariable.many))
        .map {
          case (None, None) => {
            PipoNamespace(PipoIdentifier("NaN"), Nil)
          }
          case (None, Some(a)) => {
            PipoNamespace(PipoIdentifier("NaN"), Nil)
          }
          case (Some(name), None)            => PipoNamespace(name, Nil)
          case (Some(name), Some(variables)) => PipoNamespace(name, variables)
        }

    def pField: Parser[PipoField] =
      pNamespace.map(PipoField)

    def pGrammar: Parser[PipoGrammar] =
      pField.many.debug.map(PipoGrammar)

    pGrammar.run(s)
  }

  def apply(): Parser[PipoExpr] = pipoParser
}
