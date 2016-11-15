package com.paxata.parsercombinators

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.path

@RunWith(classOf[JUnitRunner])
class ParsersTest extends path.FunSpec with StringParsers {

  describe("StringParsers") {
    val string = "theString"
    val subject: Parser[String] = exactMatchString(string)

    describe("exactMatchString") {
      it("succeeds if the start of the input matches the string exactly") {
        val input = string + "moar bytes!"

        val result = subject.apply(input).right.get

        assert(string === result.value)
        assert("moar bytes!" === result.remaining)
      }

      it("returns an error if the input does not start with the string") {
        val input = "theStrinAAAAAAAAA"
        val result = subject.apply(input).left.get
        assert(9 === result.location)
      }
    }

    describe("anyChars") {
      val subject = anyChars('a', 'b', 'c')

      it("returns success as long as input matches any characters") {
        val result = subject.apply("aaaabbbbbccccaaaFFFFF").right.get
        assert("aaaabbbbbccccaaa" === result.value)
        assert("FFFFF" === result.remaining)
      }

      it("returns error if the first character does not match the input") {
        val result = subject.apply("Fabc").left.get
        assert(0 === result.location)
      }
    }
  }

  describe("parser combinators") {

    describe("oneOf") {

      it("selects the result of the first parser that returns a success") {
        val p1 = exactMatchString("A")
        val p2 = exactMatchString("B")
        val p3 = exactMatchString("BBBBB")

        val subject = oneOf(p1, p2, p3)

        val result = subject.apply("BBBBBBBBB").right.get
        assert("B" === result.value)
        assert("BBBBBBBB" === result.remaining)
      }

      it("returns the error that most closely matched the input when all parsers fail") {
        val p1 = exactMatchString("aaaaa")
        val p2 = exactMatchString("abbbbb")
        val p3 = exactMatchString("abcde")

        val subject = oneOf(p1, p2, p3)

        val result = subject.apply("abcc").left.get
        assert(4 == result.location)
      }
    }

    describe("map") {
      it("returns the result of the map function if the parse is successful") {
        val parser = anyChars('a', 'b', 'c')

        val countAs = (string: String) => {
          string.count(_ == 'a')
        }
        val subject = map[String, Int](parser){string =>
          countAs(string)
        }

        val result = subject.apply("bacabbaGGG").right.get
        assert(3 === result.value)
        assert("GGG" === result.remaining)
      }
    }

    describe("many0") {
      val subject = many0(exactMatchString("G"))

      it("returns success for 0 matches") {
        val input = "lksdjfklasdjf"
        val result = subject.apply(input).right.get
        assert(result.value.isEmpty)
        assert(input === result.remaining)
      }

      it("returns success for multiple matches") {
        val input = "GGGyyy"
        val result = subject.apply(input).right.get
        assert(Seq("G", "G", "G") === result.value)
        assert("yyy" === result.remaining)
      }
    }

    describe("both") {

      it("returns a tuple of values on success") {
        val subject = both(exactMatchString("->"), anyChars('1', '2', '3'))
        val result = subject.apply("->32123HH").right.get
        assert(("->", "32123") === result.value)
        assert("HH" === result.remaining)
      }
    }

    describe("json") {

      val jsonString = map(sequence(exactMatch('"'), anyExcept('"'), exactMatch('"'))){values =>
        values(1) // take just the middle value
      }

      val digit = '0'.to('9')

      val jsonInt = map(charRange(digit))(_.toInt)

      val jsonFloat = map(
        sequence(
          charRange(digit),
          exactMatch('.'),
          charRange(digit)
        )
      ){sequence =>
        sequence.mkString.toDouble
      }

      it("parses a json string") {
        val input = "\"theString\""
        val result = jsonString.apply(input).right.get
        assert("theString" === result.value)
      }

      it("parses an integer") {
        val input = "12345"
        val result = jsonInt.apply(input).right.get
        assert(12345 === result.value)
      }

      it("parses a float") {
        val result = jsonFloat("123.34").right.get
        assert(123.34 === result.value)
      }
    }
  }

}

