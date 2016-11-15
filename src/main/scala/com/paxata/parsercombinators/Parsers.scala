package com.paxata.parsercombinators

import scala.collection.immutable.NumericRange

trait Parsers {

  type Input

  def advance(input: Input, amount: Int): Input

  type ParseResult[T] = Either[ParseError[Input], ParseSuccess[Input, T]]

  type Parser[T] = (Input) => ParseResult[T]

  def run[T](input: Input, parser: Parser[T]): T = ???

  def oneOf[T](parsers: Parser[T]*): Parser[T] = {
    (input: Input) => {
      val results = parsers.toStream.map{parser =>
        parser.apply(input)
      }

      results.find(_.isRight).getOrElse(results.maxBy(_.left.get.location))
    }
  }

  def map[T, R](parser: Parser[T])(mapper: (T) => R): Parser[R] = {
    (input: Input) => {
      parser.apply(input) match {
        case Left(error) => {
          Left(error)
        }
        case Right(success) => {
          createSuccess(success.remaining, mapper.apply(success.value))
        }
      }
    }
  }

  def map2[T, R, S](parser1: Parser[T], parser2: Parser[R])(mapper: ((T, R)) => S): Parser[S] = {
    map(both(parser1, parser2))(mapper)
  }

  def both[T, R](parser1: Parser[T], parser2: Parser[R]): Parser[(T, R)] = {
    (input: Input) => {
      parser1.apply(input) match {
        case Right(success) => {
          map(parser2){parser2Value =>
            (success.value, parser2Value)
          }.apply(success.remaining)
        }
        case Left(error) => createError(error.location)
      }
    }
  }

  def sequence[T](parsers: Parser[T]*): Parser[Seq[T]] = {
    (input: Input) => {
      var remainingInput = input
      var results = Seq[T]()
      var errorResult: Option[ParseResult[Seq[T]]] = None

      val parserIter = parsers.iterator
      while (parserIter.hasNext && errorResult.isEmpty) {
        parserIter.next().apply(remainingInput) match {
          case Right(success) => {
            remainingInput = success.remaining
            results = results :+ success.value
          }
          case Left(error) => {
            errorResult = Some(createError(error.location))
          }
        }
      }

      errorResult.getOrElse(createSuccess(remainingInput, results))
    }
  }

  def many0[T](parser: Parser[T]): Parser[Seq[T]] = {
    (input: Input) => {
      var results = Seq[T]()
      var remainingInput = input

      var finalResult: ParseResult[Seq[T]] = null
      while (finalResult == null) {
        parser.apply(remainingInput) match {
          case Right(success) => {
            results = results :+ success.value
            remainingInput = success.remaining
          }
          case Left(error) => {
            finalResult = createSuccess(remainingInput, results)
          }
        }
      }
      finalResult
    }
  }

  def many1[T](parser: Parser[T]): Parser[Seq[T]] = {
    (input: Input) => {
      var results = Seq[T]()
      var remainingInput = input

      var finalResult: ParseResult[Seq[T]] = null
      while (finalResult == null) {
        parser.apply(remainingInput) match {
          case Right(success) => {
            results = results :+ success.value
            remainingInput = success.remaining
          }
          case Left(error) if results.isEmpty => {
            finalResult = createError(error.location)
          }
          case Left(error)  => {
            finalResult = createSuccess(remainingInput, results)
          }
        }
      }
      finalResult
    }
  }


  def createError[T](location: Int): ParseResult[T] = {
    Left(ParseError(location))
  }

  def createSuccess[T](input: Input, value: T): ParseResult[T] = {
    Right(ParseSuccess(input, value))
  }
}

trait StringParsers extends Parsers {
  type Input = String

  override def advance(input: Input, amount: Int): Input = {
    input.substring(amount)
  }

  def whitespace(): Parser[String] = {
    anyChars(' ', '\t', '\n') //OK, so there's much more professional ways of doing this
  }

  def charRange(range: NumericRange.Inclusive[Char]): Parser[String] = {
    (input: Input) => {
      val result = input.toStream.takeWhile(char => range.containsTyped(char)).mkString
      if (!result.isEmpty()) {
        createSuccess(advance(input, result.length), result)
      } else {
        createError(0)
      }
    }
  }

  def exactMatch(char: Char): Parser[String] = {
    anyChars(char)
  }

  def anyExcept(chars: Char*): Parser[String] = {
    (input: Input) => {
      val result = input.toStream.takeWhile(c => !chars.contains(c)).mkString
      createSuccess(advance(input, result.length), result)
    }
  }

  def anyChars(chars: Char*): Parser[String] = {
    (input: Input) => {
      val stringVal = input.toStream.takeWhile(char => chars.contains(char)).mkString

      if (!stringVal.isEmpty) {
        createSuccess(advance(input, stringVal.length), stringVal)
      } else {
        createError(0)
      }
    }
  }

  def exactMatchString(toMatch: String): Parser[String] = {
    (input: Input) => {
      val strLen = toMatch.length()

      val matches = toMatch.toStream.zip(input.toStream).takeWhile((lr) => lr._1 == lr._2).length

      if (matches == strLen) {
        createSuccess(advance(input, matches), toMatch)
      } else {
        //location should point to the first non-matching character
        createError(matches + 1)
      }
    }
  }

}

final case class ParseError[I](location: Int)
final case class ParseSuccess[I, +T](remaining: I, value: T)


