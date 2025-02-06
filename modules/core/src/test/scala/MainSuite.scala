package pipo.core
package test

import cats._
import cats.implicits._
import pipo.core._
import pipo.core.Parser._

class MainSuite extends munit.FunSuite {
  val parser = Pipo()

  test("grammar test 1") {
    val expectedGrammar = Some(
      "",
      PipoGrammar(
        fields = List(
          PipoField(
            namespace = PipoNamespace(
              name = PipoIdentifier(get = "service_a"),
              variables = Nil
            )
          )
        )
      )
    )
    val input1 = """[service_a]"""
    // val obtainedGrammar = parser.run(input1)
    // assertEquals(expectedGrammar, obtainedGrammar)
  }

  test("grammar test 2") {
    val expectedGrammar = Some(
      "",
      PipoGrammar(
        fields = List(
          PipoField(
            namespace = PipoNamespace(
              name = PipoIdentifier(get = "service_a"),
              variables = List(
                PipoVariable(
                  key = PipoKey(get = PipoIdentifier(get = "key1")),
                  value = List(
                    PipoValue(
                      get = Right(PipoIdentifier(get = "value1"))
                    )
                  )
                ),
                PipoVariable(
                  key = PipoKey(get = PipoIdentifier(get = "key2")),
                  value = List(
                    PipoValue(
                      get = Right(PipoIdentifier(get = "value2"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    val input2 = """[service_a]
                   |key1=value1
                   |key2=value2
                   |""".stripMargin

    // val obtainedGrammar = parser.run(input2)
    // assertEquals(expectedGrammar, obtainedGrammar)
  }

  test("grammar test 3") {
    val expectedGrammar = Some(
      "",
      PipoGrammar(
        fields = List(
          PipoField(
            namespace = PipoNamespace(
              name = PipoIdentifier(get = "service_a"),
              variables = List(
                PipoVariable(
                  key = PipoKey(get = PipoIdentifier(get = "key1")),
                  value = List(
                    PipoValue(
                      get = Right(PipoIdentifier(get = "value1"))
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
                      get = Right(PipoIdentifier(get = "value1"))
                    )
                  )
                ),
                PipoVariable(
                  key = PipoKey(get = PipoIdentifier(get = "key2")),
                  value = List(
                    PipoValue(
                      get = Right(
                        PipoIdentifier(get = "hdfs://")
                      )
                    ),
                    PipoValue(
                      get = Left(
                        PipoInterpolate(
                          property = PipoProperty(
                            keys = List(
                              PipoIdentifier(get = "key1")
                            )
                          )
                        )
                      )
                    ),
                    PipoValue(
                      get = Right(
                        PipoIdentifier(get = "::")
                      )
                    ),
                    PipoValue(
                      get = Left(
                        PipoInterpolate(
                          property = PipoProperty(
                            keys = List(
                              PipoIdentifier(get = "service_a"),
                              PipoIdentifier(get = "key1")
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
    )

    val input3 = """[service_a]
                   |[service_b]
                   |""".stripMargin

    val obtainedGrammar = parser.run(input3)
    // assertEquals(expectedGrammar, obtainedGrammar)
  }

  // test("span") {
  //   val p1 = span(_ != '.')
  //
  //   val spans = Seq(
  //     (Some("", ""), p1.run("")),
  //     (Some(".", ""), p1.run(".")),
  //     (Some(".helloworld", ""), p1.run(".helloworld")),
  //     (Some(".world", "hello"), p1.run("hello.world")),
  //     (Some(".", "helloworld"), p1.run("helloworld.")),
  //     (Some(".hello.world.", ""), p1.run(".hello.world.")),
  //     (Some(".world.", "hello"), p1.run("hello.world."))
  //   )
  //
  //   spans.map { case (e, o) =>
  //     assertEquals(e, o)
  //   }
  // }

  test("span many") {
    val p = span(_ != '}').many
  }
}
