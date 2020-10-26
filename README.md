# SYaml

A YAML 1.2 syntax parser written in Scala.

This project aims to provide a pure-scala yaml processor with no dependencies to external libraries.

# About

Lexer converts YAML files to YEAST tokens (based on Haskell's [YamlReference reference implementation](https://hackage.haskell.org/package/YamlReference)).

Parser uses lexer YEAST tokens to generate a hierarchical model (structure matches [YAML Reference Parser](http://ben-kiki.org/ypaste/))

# Features

- Complete YAML 1.2 processor.
- Able to parse all examples from the [specification](http://www.yaml.org/spec/1.2/spec.html)

## On development

- Java API and libraries
- JS API and libraries
- Improve errors
- Documentation
- Model golden tests

# Usage

## Lexer (Yeast tokens output)

Example on how to iterate through YEAST tokens.

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

## Parser (Model output)

Example on how to parse a file

```scala
private def generate(yamlFile: File) = {
  val elements = YamlParser(yamlFile).parse()
  for (e <- elements) {
    println(e)
  }
}
```
