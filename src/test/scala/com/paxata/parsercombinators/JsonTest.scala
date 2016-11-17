package com.paxata.parsercombinators

import org.scalatest.path

import com.paxata.parsercombinators.Json._

class JsonTest extends path.FunSpec {
  describe("json") {

    it("parses a nested json document with all the types") {
      val input = """{
              "arrayKey": [
                "valOne",
                {"nestedArrayObj": 123.45},
                null
              ],
              "nullKey": null,
              "objKey": {
                "nestedArray": [1, 2.0, "five", null, {"foo": "bar"}],
                "emptyObj": {}
              }
           }"""

      val result = Json(input)
      assert(JsonNull === result.get("arrayKey.2"))
      assert(JsonString("five") === result.get("objKey.nestedArray.2"))
    }

    it("isn't perfect ;)") {
      /*
      This is just to highlight the classic problem with parser combinators. This limitation isn't usually a big deal, but
      it is something that must be acknowledged and dealt with. Otherwise you can open yourself to ddos attacks.
      We can break this with an object like: {"K": {"k": {"k": ... {"k": "v" } ... }}}}
       */
      val objectDepth = 1500
      val input = 0.to(objectDepth).map(_ => """{ "k": """).mkString + """ "value" """ + 0.to(objectDepth).map(_ => '}').mkString
      intercept[StackOverflowError]{
        Json(input)
      }
    }

    it("parses an empty object") {
      val input = "{}"
      val result = Json(input)
      val expected = JsonObject(Map())
      assert(expected === result)
    }

    it("parses a json object") {
      val input = """{ "myKey": "myValue", "keyTwo": 123.45 }"""
      val result = Json.run(input, jsonObject)
      val expected: JsonObject = JsonObject(Map("myKey" -> JsonString("myValue"), "keyTwo" -> JsonNumber(BigDecimal(123.45))))
      assert(expected === result)
    }

    it("parses a json array") {
      val input = """[ "thing one", { "key": "val" } , 123 ]"""
      val result = Json.run(input, jsonArray)
      val expected = JsonArray(Seq(JsonString("thing one"), JsonObject(Map("key" -> JsonString("val"))), JsonNumber(BigDecimal(123))))
      assert(expected === result)
    }

    it("parses a json string") {
      val input = "\"theString\""
      val result = jsonString.apply(input).right.get
      assert(JsonString("theString") === result.value)
    }

    it("parses an integer") {
      val input = "12345"
      val result = jsonInt.apply(input).right.get
      assert(JsonNumber(BigDecimal(12345)) === result.value)
    }

    it("parses a float") {
      val result = jsonFloat("123.34").right.get
      assert(JsonNumber(BigDecimal(123.34)) === result.value)
    }
  }

}
