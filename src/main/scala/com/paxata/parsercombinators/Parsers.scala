package com.paxata.parsercombinators

trait Parsers {

  type Input

  def advance(input: Input, amount: Int): Input

  type ParseResult[T] = Either[ParseError[Input], ParseSuccess[Input, T]]

  type Parser[T] = (Input) => ParseResult[T]

  def run[T](input: Input, parser: Parser[T]): T = ???

  def oneOf[T](parsers: Parser[T]*): ParseResult[T] = ???

  def createError[T](location: Int): ParseResult[T] = {
    Left(ParseError(location))
  }

  def createSuccess[T](input: Input, value: T): ParseResult[T] = {
    Right(ParseSuccess(input, value))
  }
}

object ByteArrayParsers extends Parsers {
  type Input = Array[Byte]

  override def advance(input: Input, amount: Int): Input = {
    input.slice(amount, input.length)
  }

  def exactMatchString(toMatch: String): Parser[String] = {
    (bytes: Input) => {
      val strLen = toMatch.length()

      val matches = toMatch.getBytes().toStream.zip(bytes.toStream).map((lr) => lr._1 == lr._2).takeWhile(_ == true).length

      if (matches == strLen) {
        createSuccess(advance(bytes, matches), toMatch)
      } else {
        createError(matches + 1) //location should point to the first non-matching character
      }
    }
  }

}

final case class ParseError[I](location: Int)
final case class ParseSuccess[I, +T](remaining: I, value: T)


