package com.paxata.parsercombinators

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.path

@RunWith(classOf[JUnitRunner])
class ParsersTest extends path.FunSpec {

  describe("Parsers") {
    
    it("foo() returns foo") {
      assert("foo" == Parsers.foo())
    }
  }
}

