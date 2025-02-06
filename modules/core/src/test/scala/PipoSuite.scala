package pipo.core
package test

import pipo.core._
import pipo.core.Parser._
import pipo.core.Pipo._

class PipoSuite extends munit.FunSuite {
  test("pGrammar 1") {
    val input = ""
    val expected = Some(
      "",
      PipoGrammar(Nil)
    )
    val obtained = pGrammar.run(input)
    assertEquals(obtained, expected)
  }

  test("pGrammar 2") {
    val input = "abcdef"
    val expected = Some(
      "abcdef",
      PipoGrammar(Nil)
    )
    val obtained = pGrammar.run(input)
    assertEquals(obtained, expected)
  }

  test("pGrammar 3") {
    val input = "[namespace_1]\n"
    val expected = Some(
      "",
      PipoGrammar(
        List(
          PipoField(
            PipoNamespace(
              PipoLit("namespace_1"),
              Nil
            )
          )
        )
      )
    )
    val obtained = pGrammar.run(input)
    assertEquals(obtained, expected)
  }

  test("pGrammar 4") {
    val input = "    [namespace_1]\n"
    val expected = Some(
      "    [namespace_1]\n",
      PipoGrammar(Nil)
    )
    val obtained = pGrammar.run(input)
    assertEquals(obtained, expected)
  }

  test("pGrammar 5") {
    val input = "\n\n\n[namespace_1]\n\n\n"
    val expected = Some(
      "",
      PipoGrammar(
        List(
          PipoField(
            PipoNamespace(
              PipoLit("namespace_1"),
              Nil
            )
          )
        )
      )
    )
    val obtained = pGrammar.run(input)
    assertEquals(obtained, expected)
  }

  test("pVariable 1") {
    val input = "key1=value1"
    val expected = Some(
      "",
      PipoVariable(
        PipoKey(PipoLit("key1")),
        PipoValue(
          PipoLit("value1")
        )
      )
    )
    val obtained = pVariable.run(input)
    assertEquals(obtained, expected)
  }

  test("pVariable 2") {
    val input = "key1=value1\nkey2=value2"
    val expected = Some(
      "",
      List(
        PipoVariable(
          PipoKey(PipoLit("key1")),
          PipoValue(
            PipoLit("value1")
          )
        ),
        PipoVariable(
          PipoKey(PipoLit("key2")),
          PipoValue(
            PipoLit("value2")
          )
        )
      )
    )
    val obtained = pVariable.many.run(input)
    assertEquals(obtained, expected)
  }

  test("pVariable 3") {
    val input = "key1=="
    val expected = Some(
      "",
      PipoVariable(
        PipoKey(PipoLit("key1")),
        PipoValue(
          PipoLit("=")
        )
      )
    )
    val obtained = pVariable.run(input)
    assertEquals(obtained, expected)
  }

  test("pVariable 4") {
    val input = "key1"
    val expected = None
    val obtained = pVariable.run(input)
    assertEquals(obtained, expected)
  }

  test("pVariable 5") {
    val input = "key1=value1\n\nkey2=value2"
    val expected = Some(
      "",
      List(
        PipoVariable(
          PipoKey(PipoLit("key1")),
          PipoValue(
            PipoLit("value1")
          )
        ),
        PipoVariable(
          PipoKey(PipoLit("key2")),
          PipoValue(
            PipoLit("value2")
          )
        )
      )
    )
    val obtained = pVariable.many.run(input)
    assertEquals(obtained, expected)
  }

  test("pVariable 6") {
    val input = "key1=value1\n\nkey2=value2\n\n\n\n\n\nkey3=value3"
    val expected = Some(
      "",
      List(
        PipoVariable(
          PipoKey(PipoLit("key1")),
          PipoValue(
            PipoLit("value1")
          )
        ),
        PipoVariable(
          PipoKey(PipoLit("key2")),
          PipoValue(
            PipoLit("value2")
          )
        ),
        PipoVariable(
          PipoKey(PipoLit("key3")),
          PipoValue(
            PipoLit("value3")
          )
        )
      )
    )
    val obtained = pVariable.many.run(input)
    assertEquals(obtained, expected)
  }

  test("pField 1") {
    val input = "[namespace_1]\nkey1=value1"
    val expected = Some(
      (
        "",
        PipoField(
          PipoNamespace(
            PipoLit("namespace_1"),
            List(
              PipoVariable(
                PipoKey(PipoLit("key1")),
                PipoValue(
                  PipoLit("value1")
                )
              )
            )
          )
        )
      )
    )

    val obtained = pField.run(input)
    assertEquals(obtained, expected)
  }

  test("pField 2") {
    val input = "[namespace_1]\nkey1=value1\nkey2=value2"
    val expected = Some(
      (
        "",
        PipoField(
          PipoNamespace(
            PipoLit("namespace_1"),
            List(
              PipoVariable(
                PipoKey(PipoLit("key1")),
                PipoValue(
                  PipoLit("value1")
                )
              ),
              PipoVariable(
                PipoKey(PipoLit("key2")),
                PipoValue(
                  PipoLit("value2")
                )
              )
            )
          )
        )
      )
    )

    val obtained = pField.run(input)
    assertEquals(obtained, expected)
  }

  test("pField 3") {
    val input = "[namespace_1]\nkey1=value1\n\n[namespace_2]\nkey2=value2"
    val expected = Some(
      (
        "",
        List(
          PipoField(
            PipoNamespace(
              PipoLit("namespace_1"),
              List(
                PipoVariable(
                  PipoKey(PipoLit("key1")),
                  PipoValue(
                    PipoLit("value1")
                  )
                )
              )
            )
          ),
          PipoField(
            PipoNamespace(
              PipoLit("namespace_2"),
              List(
                PipoVariable(
                  PipoKey(PipoLit("key2")),
                  PipoValue(
                    PipoLit("value2")
                  )
                )
              )
            )
          )
        )
      )
    )

    val obtained = pField.many.run(input)
    assertEquals(obtained, expected)
  }

  test("pField 4") {
    val input =
      "\n\n\n[namespace_1]\nkey1=value1\n\n[namespace_2]\nkey2=value2\n\n\n[namespace_3]\nkey3=value3\nkey4=value4\nkey5=value5\n\n\n\n\n\n\n\n[namespace_4]\nkey1=value1\nkey2=value2\nkey3=value3\n\n\n\n"
    val expected = Some(
      (
        "",
        List(
          PipoField(
            PipoNamespace(
              PipoLit("namespace_1"),
              List(
                PipoVariable(
                  PipoKey(PipoLit("key1")),
                  PipoValue(
                    PipoLit("value1")
                  )
                )
              )
            )
          ),
          PipoField(
            PipoNamespace(
              PipoLit("namespace_2"),
              List(
                PipoVariable(
                  PipoKey(PipoLit("key2")),
                  PipoValue(
                    PipoLit("value2")
                  )
                )
              )
            )
          ),
          PipoField(
            PipoNamespace(
              PipoLit("namespace_3"),
              List(
                PipoVariable(
                  PipoKey(PipoLit("key3")),
                  PipoValue(
                    PipoLit("value3")
                  )
                ),
                PipoVariable(
                  PipoKey(PipoLit("key4")),
                  PipoValue(
                    PipoLit("value4")
                  )
                ),
                PipoVariable(
                  PipoKey(PipoLit("key5")),
                  PipoValue(
                    PipoLit("value5")
                  )
                )
              )
            )
          ),
          PipoField(
            PipoNamespace(
              PipoLit("namespace_4"),
              List(
                PipoVariable(
                  PipoKey(PipoLit("key1")),
                  PipoValue(
                    PipoLit("value1")
                  )
                ),
                PipoVariable(
                  PipoKey(PipoLit("key2")),
                  PipoValue(
                    PipoLit("value2")
                  )
                ),
                PipoVariable(
                  PipoKey(PipoLit("key3")),
                  PipoValue(
                    PipoLit("value3")
                  )
                )
              )
            )
          )
        )
      )
    )

    val obtained = pField.many.run(input)
    assertEquals(obtained, expected)
  }

}
