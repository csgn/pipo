package pipo.core

import cats._
import cats.implicits._

case class Parser[+A](run: String => Option[(String, A)])
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

  def predicate(f: Char => Boolean): Parser[String] = Parser { s =>
    @scala.annotation.tailrec
    def loop(str: String)(rest: String, token: String): (String, String) = {
      if (str.isEmpty) (rest, token)
      else {
        val (ch, tail) = (str.head, str.tail)

        if (!f(ch)) (rest + ch + tail, token)
        else loop(tail)(rest, token + ch)
      }
    }

    Some(loop(s)("", ""))
  }

  implicit class ParserOps[A](p1: Parser[A])(implicit F: Alternative[Parser]) {
    final def or(p2: Parser[A]): Parser[A] = F.combineK(p1, p2)
    final def <|>(p2: Parser[A]): Parser[A] = or(p2)

    final def map[B](f: A => B): Parser[B] = F.map(p1)(f)

    final def as[B](b: B): Parser[B] = F.as(p1, b)

    final def <*[B](p2: Parser[B]): Parser[A] = F.productL(p1)(p2)
    final def *>[B](p2: Parser[B]): Parser[B] = F.productR(p1)(p2)
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
