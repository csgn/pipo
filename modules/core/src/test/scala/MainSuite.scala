package pipo.core
package test

import cats._
import cats.implicits._
import pipo.core._
import pipo.core.Parser._

class MainSuite extends munit.FunSuite {
  test("grammar test 1") {
    val expected1 = PipoGrammar(
      fields = List(
        PipoField(
          namespace = PipoNamespace(
            name = PipoIdentifier(get = "service_a"),
            variables = Nil
          )
        )
      )
    )
    val input1 = """
    [service_a]
    """
  }

  test("grammar test 2") {
    val expected2 = PipoGrammar(
      fields = List(
        PipoField(
          namespace = PipoNamespace(
            name = PipoIdentifier(get = "service_a"),
            variables = List(
              PipoVariable(
                key = PipoKey(get = PipoIdentifier(get = "key1")),
                value = List(
                  PipoValue(
                    get = Left(PipoIdentifier(get = "value1"))
                  )
                )
              ),
              PipoVariable(
                key = PipoKey(get = PipoIdentifier(get = "key2")),
                value = List(
                  PipoValue(
                    get = Left(PipoIdentifier(get = "value2"))
                  )
                )
              ),
              PipoVariable(
                key = PipoKey(get = PipoIdentifier(get = "key3")),
                value = List(
                  PipoValue(
                    get = Left(PipoIdentifier(get = "value3"))
                  )
                )
              )
            )
          )
        )
      )
    )

    val input2 = """
    [service_a]
    key1=value1
    key2=value2
    key3=value3
    """
  }

  test("grammar test 3") {
    val expected3 = PipoGrammar(
      fields = List(
        PipoField(
          namespace = PipoNamespace(
            name = PipoIdentifier(get = "service_a"),
            variables = List(
              PipoVariable(
                key = PipoKey(get = PipoIdentifier(get = "key1")),
                value = List(
                  PipoValue(
                    get = Left(PipoIdentifier(get = "value1"))
                  )
                )
              )
            )
          )
        ),
        PipoField(
          namespace = PipoNamespace(
            name = PipoIdentifier(get = "service_b"),
            variables = List(
              PipoVariable(
                key = PipoKey(get = PipoIdentifier(get = "key2")),
                value = List(
                  PipoValue(
                    get = Left(PipoIdentifier(get = "value1"))
                  )
                )
              ),
              PipoVariable(
                key = PipoKey(get = PipoIdentifier(get = "key2")),
                value = List(
                  PipoValue(
                    get = Left(
                      PipoIdentifier(get = "hdfs://")
                    )
                  ),
                  PipoValue(
                    get = Right(
                      PipoInterpolate(
                        property = PipoProperty(
                          keys = List(
                            PipoKey(get = PipoIdentifier(get = "key1"))
                          )
                        )
                      )
                    )
                  ),
                  PipoValue(
                    get = Left(
                      PipoIdentifier(get = "::")
                    )
                  ),
                  PipoValue(
                    get = Right(
                      PipoInterpolate(
                        property = PipoProperty(
                          keys = List(
                            PipoKey(get = PipoIdentifier(get = "service_a")),
                            PipoKey(get = PipoIdentifier(get = "key1"))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    val input3 = """
    [service_a]
    key1=value1

    [service_b]
    key1=value1
    key2=hdfs://{{key1}}::{{service_a.key1}}
    """
  }

  test("predicate") {
    val p1 = predicate(_ != '.')

    val predicates = Seq(
      (Some("", ""), p1.run("")),
      (Some(".", ""), p1.run(".")),
      (Some(".helloworld", ""), p1.run(".helloworld")),
      (Some(".world", "hello"), p1.run("hello.world")),
      (Some(".", "helloworld"), p1.run("helloworld.")),
      (Some(".hello.world.", ""), p1.run(".hello.world.")),
      (Some(".world.", "hello"), p1.run("hello.world."))
    )

    predicates.map { case (e, o) =>
      assertEquals(e, o)
    }
  }

  test("predicate or") {
    val p1 = predicate(ch => List('.', ',', 'r').contains(ch))

    println(p1.run("hello.world,"))
  }
}
