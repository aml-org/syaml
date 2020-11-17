# SYaml

This project provides a Pure Scala YAML processor that has no dependencies on external libraries.

SYaml is a YAML 1.2 syntax parser written in Scala.


# About

The lexer converts YAML files to YEAST tokens using Haskell's [YamlReference: YAML reference implementation](https://hackage.haskell.org/package/YamlReference)).

The parser uses the lexer YEAST token output to generate a hierarchical model.
The structure of the model matches the structure created by the [YAML Reference Parser](http://ben-kiki.org/ypaste/)).

# Features

- Complete YAML 1.2 processor
- Able to parse all examples from the [YAML specification](http://www.yaml.org/spec/1.2/spec.html).

## In development

- Java API and libraries
- JavaScript API and libraries
- Improve error messages
- Improve documentation
- Model golden tests

# Usage

## Lexer (YEAST Token Output)

The following example shows how to iterate through YEAST tokens:

```scala
private def generate(yamlFile: File, yeastFile: File) = {
  val out   = new PrintWriter(yeastFile)
  val lexer = YamlLexer(yamlFile)
  while (lexer.token != YamlToken.EndStream) {
    val data = YeastData(lexer.tokenData, lexer.tokenString)
    out.println(data)
    lexer.advance()
  }
  out.close()
}
```

## Parser (Model Output)

The following example shows how to parse a file:

```scala
private def generate(yamlFile: File) = {
  val elements = YamlParser(yamlFile).parse()
  for (e <- elements) {
    println(e)
  }
}
```
