package pipo.core

import cats._
import cats.implicits._

case class Parser[+A](run: String => Option[(String, A)])

object Symbol {
  val Equal = '='
  val Newline = '\n'
  val OpenBracket = '['
  val CloseBracket = ']'
  val Whitespace = ' '
}

object Parser {
  def char(c: Char): Parser[Char] = Parser { s =>
    if (s.isEmpty || s.charAt(0) != c) None
    else Some(s.tail, c)
  }

  def string(x: String): Parser[String] = Parser { s =>
    x.map(char).toList.sequence.run(s).map { case (x, y) =>
      (x, y.mkString)
    }
  }

  def nl: Parser[List[Char]] = char(Symbol.Newline).many

  def ws: Parser[List[Char]] = char(Symbol.Whitespace).many

  def until(chars: Char*): Parser[String] = span(x => !chars.contains(x))

  def span(f: Char => Boolean): Parser[String] = Parser { s =>
    @scala.annotation.tailrec
    def loop(str: String)(rest: String, token: String): (String, String) = {
      if (str.isEmpty) (rest, token)
      else {
        val (ch, tail) = (str.head, str.tail)

        if (!f(ch)) (rest + ch + tail, token)
        else loop(tail)(rest, token + ch)
      }
    }

    val (rest, token) = loop(s)("", "")
    if (token.isEmpty) None
    else Some((rest, token))
  }

  implicit class ParserOps[A](p1: Parser[A])(implicit F: Alternative[Parser]) {
    final def or(p2: Parser[A]): Parser[A] = F.combineK(p1, p2)
    final def <|>(p2: Parser[A]): Parser[A] = or(p2)

    final def orThen[B](p2: => Parser[B]): Parser[Either[A, B]] = Parser { s =>
      p1.run(s) match {
        case None =>
          p2.run(s) match {
            case None          => None
            case Some((s2, b)) => Some((s2, Right(b)))
          }
        case Some((s2, a)) => Some((s2, Left(a)))
      }
    }
    final def <||>[B](p2: => Parser[B]): Parser[Either[A, B]] = orThen(p2)

    final def map[B](f: A => B): Parser[B] = F.map(p1)(f)

    final def <*[B](p2: => Parser[B]): Parser[A] = F.productL(p1)(p2)
    final def *>[B](p2: => Parser[B]): Parser[B] = F.productR(p1)(p2)

    def many: Parser[List[A]] = {
      def loop(acc: => List[A]): Parser[List[A]] = Parser { s =>
        p1.run(s) match {
          case None          => Some((s, acc))
          case Some((s2, a)) => loop(acc :+ a).run(s2)
        }
      }
      loop(List.empty[A])
    }

    final def forwardTo[B](p2: => Parser[B]): Parser[(Option[A], Option[B])] = Parser { s =>
      p1.run(s) match {
        case None =>
          p2.run(s) match {
            case None          => None
            case Some((s3, b)) => Some((s3, (None, Some(b))))
          }
        case Some((s2, a)) =>
          p2.run(s2) match {
            case None          => Some((s2, (Some(a), None)))
            case Some((s3, b)) => Some((s3, (Some(a), Some(b))))
          }
      }
    }

    final def ~[B](p2: => Parser[B]): Parser[(Option[A], Option[B])] = forwardTo(p2)

    final def forwardToIfLeftSome[B](p2: => Parser[B]): Parser[(Option[A], Option[B])] = Parser { s =>
      p1.run(s) match {
        case None => None
        case Some((s2, a)) =>
          p2.run(s2) match {
            case None          => Some((s2, (Some(a), None)))
            case Some((s3, b)) => Some((s3, (Some(a), Some(b))))
          }
      }
    }

    final def !~[B](p2: => Parser[B]): Parser[(Option[A], Option[B])] = forwardToIfLeftSome(p2)

    final def debug(s: String = ""): Parser[A] = {
      F.map(p1)(a => {
        println(s"==DEBUG${if (s.isEmpty) s else s"[${s}]"}=========")
        println(a.toString.replaceAll("\\n", "\\\\n"))
        println("----------------\n")
        a
      })
    }

    final def debug: Parser[A] = debug()
  }

  implicit val parserAlternative: Alternative[Parser] = new Alternative[Parser] {
    def pure[A](a: A): Parser[A] = Parser(s => Some(s, a))

    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = Parser { s =>
      for {
        (s1, f) <- ff.run(s)
        (s2, a) <- fa.run(s1)
      } yield (s2, f(a))
    }

    def empty[A]: Parser[A] = Parser(_ => None)

    def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = Parser { s =>
      x.run(s) match {
        case None     => y.run(s)
        case some @ _ => some
      }
    }
  }
}
