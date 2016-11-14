package com.paxata.parsercombinators

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.path

import scala.util.Either.RightProjection

@RunWith(classOf[JUnitRunner])
class ParsersTest extends path.FunSpec {

  describe("ByteArrayParsers") {
    val string = "theString"
    val subject = ByteArrayParsers.exactMatchString(string)

    describe("exactMatchString") {
      it("succeeds if the start of the input matches the string exactly") {
        val input = (string + "moar bytes!").getBytes()

        val result = subject.apply(input).right.get

        assert(string === result.value)
        assert("moar bytes!".getBytes() === result.remaining)
      }

      it("returns an error if the input does not start with the string") {
        val input = "theStrinAAAAAAAAA".getBytes()
        val result = subject.apply(input).left.get
        assert(9 === result.location)
      }
    }
  }
}

