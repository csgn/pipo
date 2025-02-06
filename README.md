# pipo

<p align="center">
    <img src="/docs/icon.jpg" width="256" height="256" />
</p>

# Grammar
```ebnf
grammar = { field } ;
field = namespace , newline, variables ;
namespace = "[" , lit , "]", newline ;
variables = { variable } ;
variable = key, '=', value, newline
key = lit ;
value = lit ;
lit = letter , { letter | digit | "_" } ;

character = letter | digit | symbol | "_" ;

letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" ;
       
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
       | "'" | '"' | "=" | "|" | "." | "," | ";" ;

newline = "\n" ;
```

# Example
```
# Source
[namespace_1]
key1=value1
key2=value2

# AST
PipoGrammar(
    List(
        PipoField(
            PipoNamespace(
                PipoLit(namespace_1),
                List(
                    PipoVariable(
                        PipoKey(
                            PipoLit(key1)
                        ),
                        PipoValue(
                            PipoLit(value1)
                        )
                    ),
                    PipoVariable(
                        PipoKey(
                            PipoLit(key2)
                        ),
                        PipoValue(
                            PipoLit(value2)
                        )
                    ),
                    PipoVariable(
                        PipoKey(
                            PipoLit(key3)
                        ),
                        PipoValue(
                            PipoLit(value3)
                        )
                    )
                )
            )
        )
    )
)
```

# Usage
```scala
import pipo.core._

val input = """[namespace_1]
              |key1=value1
              |key2=value2
              |""".stripMargin

val parser = Pipo()
val Some((_, ast)) = parser.run(input)
```

# Run tests
```sh
$ sbt test
```
