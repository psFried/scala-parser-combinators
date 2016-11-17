package com.paxata.parsercombinators

import scala.collection.immutable.NumericRange

/**
  * Parsers are just functions that take some input and return a special kind of result. The result needs to have a few properties.
  * First, it needs to tell us how much of the input was consumed, or else just return a new Input instance for us to
  * pass into subsequent parsers. Second, it should either contain the value that was successfully parsed, or else an
  * error value that gives some sort of information about where the problem is.
  *
  * The Parsers trait is totally generic. It contains only abstractions that allow parsers to be composed, but does not
  * actually define the parsers themselves. Concrete implementations of parsers will define their own set of primitive
  * parsers, depending on what sort of input they can deal with.
  */
trait Parsers {

  /**
    * Represents the type of input that the parser will operate on. Most parsers would probably operate on a byte[],
    * but some might also just accept some sort of CharSequence/String
    */
  type Input

  // One of the primitives that is needed in order to compose parsers
  def advance(input: Input, amount: Int): Input

  def render(input: Input, errorPosition: Int): String

  //Parsers always return a value. They never throw exceptions. This allows us to render better error messages.
  //Other implementations of ParseResults allow for things like decoupling parsing from the underlying IO, which
  // can be really nice.
  type ParseResult[+T] = Either[ParseError[Input], ParseSuccess[Input, T]]

  //A parser is just a function that takes an Input (whatever that may be) and returns a Result that contains
  // either the successful result or else an error message
  type Parser[+T] = (Input) => ParseResult[T]

  //Runs a parser on the given input. Returns the result of a successful parse, or throws an exception.
  def run[T](input: Input, parser: Parser[T]): T = {
    parser.apply(input) match {
      case Left(error) => {
        throw new Exception(render(error.input, error.location))
      }
      case Right(success) => success.value
    }
  }


  /*
  Combinators
  ===========

  Combinators are totally generic, and simply provide ways for us to compose parsers.
   */

  //One of the most common ways to compose parsers. Takes a list of parsers and returns the result of whichever
  // one is successfull
  def oneOf[T](parsers: Parser[T]*): Parser[T] = {
    (input: Input) => {
      val results = parsers.toStream.map{parser =>
        parser.apply(input)
      }

      results.find(_.isRight).getOrElse(results.maxBy(_.left.get.location))
    }
  }

  //good old map. Just like mapping an option
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

  //flatMap is a critical component because it allows parsing context-sensitive grammars
  def flatMap[T, R](parser: Parser[T])(returnsParser: (T) => Parser[R]): Parser[R] = {
    (input: Input) => {
      parser.apply(input) match {
        case Left(error) => {
          createError(input, error.location)
        }
        case Right(success) => {
          val newParser: Parser[R] = returnsParser.apply(success.value)
          newParser.apply(success.remaining)
        }
      }
    }
  }

  //just ignores a failure of the given parser
  def optional[T](parser: Parser[T]): Parser[Option[T]] = {
    (input: Input) => {
      parser.apply(input) match {
        case Left(_) => createSuccess(input, None)
        case Right(success) => createSuccess(success.remaining, Some(success.value))
      }
    }
  }

  // just a good old fashioned bi-combinator
  def map2[T, R, S](parser1: Parser[T], parser2: Parser[R])(mapper: ((T, R)) => S): Parser[S] = {
    map(both(parser1, parser2))(mapper)
  }

  //default bi-combinator that returns a tuple of the two values
  def both[T, R](parser1: Parser[T], parser2: Parser[R]): Parser[(T, R)] = {
    (input: Input) => {
      parser1.apply(input) match {
        case Right(success) => {
          map(parser2){parser2Value =>
            (success.value, parser2Value)
          }.apply(success.remaining)
        }
        case Left(error) => createError(input, error.location)
      }
    }
  }

  /**
    * Perhaps the most complex combinator we have here, chain allows you to map the results of a sequence of parsers.
    * Using a Seq[Any] isn't the most elegant solution. The popular alternative in Scala land would be to just generate
    * a bunch of mapN implementations, which would allow the type system to help you out when using them. I'm lazy, though,
    * so I just used a Seq[Any] :)
    */
  def chain[R](parsers: Parser[Any]*)(mapper: (Seq[Any]) => R): Parser[R] = {
    (input: Input) => {
      var remainingInput = input
      var results = Seq[Any]()
      var errorResult: Option[ParseResult[R]] = None

      val parserIter = parsers.iterator
      while (parserIter.hasNext && errorResult.isEmpty) {
        parserIter.next().apply(remainingInput) match {
          case Right(success) => {
            remainingInput = success.remaining
            results = results :+ success.value
          }
          case Left(error) => {
            errorResult = Some(createError(remainingInput, error.location))
          }
        }
      }

      errorResult.getOrElse(createSuccess(remainingInput, mapper.apply(results)))
    }
  }

  //apply the given parsers in order and return a sequence with their results
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
            errorResult = Some(createError(remainingInput, error.location))
          }
        }
      }

      errorResult.getOrElse(createSuccess(remainingInput, results))
    }
  }

  //apply the given parser 0 or more times, yielding a sequence of the results
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

  //apply the given parser 1 or more times, yielding a sequence of the results
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
            finalResult = createError(remainingInput, error.location)
          }
          case Left(error)  => {
            finalResult = createSuccess(remainingInput, results)
          }
        }
      }
      finalResult
    }
  }

  //apply the given parser 0 or more times, requiring the given separator parser to be applied in between
  def many0Separated[T, R](repeatParser: Parser[T], separator: Parser[R]): Parser[Seq[T]] = {
    (input: Input) => {
      var results = Seq[T]()
      var remainingInput = input
      var finalResult: ParseResult[Seq[T]] = null
      while (finalResult == null) {
        repeatParser.apply(remainingInput) match {
          case Left(error) => {
            finalResult = createSuccess(remainingInput, results)
          }
          case Right(success) => {
            results = results :+ success.value
            separator.apply(success.remaining) match {
              case Left(error) => finalResult = createSuccess(success.remaining, results)
              case Right(separatorSuccess) => remainingInput = separatorSuccess.remaining
            }
          }
        }
      }

      finalResult
    }
  }


  // Helper methods for creating ParseResults

  def createError[T](input: Input, location: Int): ParseResult[T] = {
    Left(ParseError(input, location))
  }

  def createSuccess[T](input: Input, value: T): ParseResult[T] = {
    Right(ParseSuccess(input, value))
  }
}

/**
  * Example implementation of the Parsers trait. It provides the basic Parsers that can be composed using the methods
  * defined in the Parsers trait.
  */
trait StringParsers extends Parsers {
  type Input = String

  override def advance(input: Input, amount: Int): Input = {
    /*
    Ok, time for a real heart to heart about what we're doing here. This part really isn't good.
    The String.substring method copies the underlying char array every time it's called. It didn't used to do that, but
    changed sometime in one of the 1.7 releases. Strings used to store an offset and length that were applied to the
    backing char[]. That meant that calling substring would just create a new String object that way backed by the same
    char[] as the previous one, just with different offsets.

    If this were real production code, we'd want to implement our own CharSequence that used something more like the
    old behavior. That would mean we'd only have one copy of the actual string data. As it is, this is super inefficient,
    but I just can't be bothered to fix it since it's just an example.
     */
    input.substring(amount)
  }


  override def render(input: String, errorPosition: Int): String = {
    s"Error: [${input.substring(Math.min(errorPosition, 0), Math.max(errorPosition, 1))}]${input.substring(Math.max(errorPosition, 1))}"
  }

  //matches 1 or more whitespace characters
  def whitespace(): Parser[String] = {
    anyChars(' ', '\t', '\n') //OK, so there's much more professional ways of doing this
  }

  /**
    * Matches one or more characters within the given range. We could have implemented this using a Parser that just matches
    * a single character at a time and wrapped it in a many1, but it was done this way for the sake of efficiency when
    * working with strings.
    * @param range in scala, a char range is created as: `'a'.to('z')`
    * @return
    */
  def charRange(range: NumericRange.Inclusive[Char]): Parser[String] = {
    (input: Input) => {
      val result = input.toStream.takeWhile(char => range.containsTyped(char)).mkString
      if (!result.isEmpty()) {
        createSuccess(advance(input, result.length), result)
      } else {
        createError(input, 0)
      }
    }
  }

  def exactMatch(char: Char): Parser[String] = {
    exactMatchString(char.toString())
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
        createError(input, 0)
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
        createError(input, matches + 1)
      }
    }
  }

}

final case class ParseError[I](input: I, location: Int)
final case class ParseSuccess[I, +T](remaining: I, value: T)


