package com.paxata.parsercombinators


sealed trait JsonValue {
  def get(dotSeparatedString: String): JsonValue = {
    dotSeparatedString.split('.').foldLeft(this){(jsonValue, pathSegment) =>
      jsonValue.getValue(pathSegment)
    }
  }

  def getValue(key: String): JsonValue = {
    this match {
      case JsonObject(map) => map.get(key).orNull
      case JsonArray(seq) => seq(key.toInt)
      case _ => throw new Exception(s"Can't get value on: ${this}")
    }
  }
}

final case class JsonNumber(num: BigDecimal) extends JsonValue
final case class JsonString(value: String) extends JsonValue
final case class JsonArray(values: Seq[JsonValue]) extends JsonValue
final case class JsonObject(values: Map[String, JsonValue]) extends JsonValue
case object JsonNull extends JsonValue

object Json extends StringParsers {

  def apply(input: Input): JsonValue = {
    run(input, _jsonValue())
  }

  val string = map(sequence(exactMatch('"'), anyExcept('"'), exactMatch('"'))){values =>
    values(1) // take just the middle value
  }

  val jsonString = map(string){value => JsonString(value) }

  val digit = '0'.to('9')

  val jsonInt = map(charRange(digit)){num =>
    JsonNumber(BigDecimal(num))
  }

  val jsonFloat = map(sequence(charRange(digit), exactMatch('.'), charRange(digit))){sequence =>
    JsonNumber(BigDecimal(sequence.mkString))
  }

  val jsonNumber = oneOf(jsonFloat, jsonInt)

  val jsonNull = map[String, JsonValue](exactMatchString("null"))(_ => JsonNull)

  val optWhitespace = optional(whitespace())

  val commaSeparator = sequence(optWhitespace, exactMatch(','), optWhitespace)

  val jsonValue = (input: Input) => {
    _jsonValue().apply(input)
  }

  val jsonKeyValue: Parser[(String, JsonValue)] = chain(
    optWhitespace,
    string,
    optWhitespace,
    exactMatch(':'),
    optWhitespace,
    jsonValue,
    optWhitespace
  ){values =>
    (values(1).asInstanceOf[String], values(5).asInstanceOf[JsonValue])
  }

  val jsonObject: Parser[JsonValue] = chain(exactMatch('{'), optWhitespace, many0Separated(jsonKeyValue, commaSeparator), optWhitespace, exactMatch('}') ){ values =>
    val kvPairs = values(2).asInstanceOf[Seq[(String, JsonValue)]]
    JsonObject(kvPairs.toMap)
  }

  val jsonArray: Parser[JsonValue] = chain(
    exactMatch('['),
    optWhitespace,
    many0Separated(jsonValue, commaSeparator),
    optWhitespace,
    exactMatch(']')
  ){values =>
    JsonArray(values(2).asInstanceOf[Seq[JsonValue]])
  }

  //this one is a method instead of a val so that we don't have a bunch of weird forward references in vals
  private def _jsonValue(): Parser[JsonValue] = oneOf[JsonValue](jsonString, jsonNumber, jsonObject, jsonArray, jsonNull)
}
