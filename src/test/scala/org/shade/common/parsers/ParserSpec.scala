package org.shade.common.parsers

import org.scalatest.{Matchers, WordSpec}


class ParserSpec extends WordSpec with Matchers {

  import ParserSpec.OneOrTwo

  "unapply" should {

    "correctly extract valid values" in {

      "one" match {
        case OneOrTwo.Parsed(n) => n shouldBe OneOrTwo(1)
        case _ => fail("Expected match")
      }

      "two" match {
        case OneOrTwo.Parsed(n) => n shouldBe OneOrTwo(2)
        case _ => fail("Expected match")
      }
    }

    "fail to extract invalid values" in {

      "three" match {
        case OneOrTwo.Parsed(n) => fail("Expected no match")
        case _ => // Success
      }

      null match {
        case OneOrTwo.Parsed(n) => fail("Expected no match")
        case _ => // Success
      }
    }
  }
}

object ParserSpec {

  private case class OneOrTwo(n: Int)

  private object OneOrTwo extends Parser[OneOrTwo] {

    override def parse(str: String): Option[OneOrTwo] = {
      str match {
        case "one" => Some(OneOrTwo(1))
        case "two" => Some(OneOrTwo(2))
        case _ => None
      }
    }
  }
}
