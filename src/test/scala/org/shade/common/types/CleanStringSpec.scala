package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class CleanStringSpec extends WordSpec with Matchers {

  "Construction" should {

    "succeed for non-empty strings without leading/trailing whitespace" in {

      CleanString("abc").value shouldBe "abc"
      CleanString("a").value shouldBe "a"
      CleanString("xx xx").value shouldBe "xx xx"
    }

    "fail for an empty string" in {
      an[IllegalArgumentException] should be thrownBy CleanString("")
    }

    "fail for a blank string or a string with leading/trailing spaces" in {
      an[IllegalArgumentException] should be thrownBy CleanString(" ")
      an[IllegalArgumentException] should be thrownBy CleanString("    ")
      an[IllegalArgumentException] should be thrownBy CleanString("\n")
      an[IllegalArgumentException] should be thrownBy CleanString("\t")
      an[IllegalArgumentException] should be thrownBy CleanString(" abc")
      an[IllegalArgumentException] should be thrownBy CleanString("abc ")
      an[IllegalArgumentException] should be thrownBy CleanString(" abc ")
      an[IllegalArgumentException] should be thrownBy CleanString("abc\n")
    }
  }

  "of" should {

    "succeed for non-empty strings without leading/trailing whitespace" in {

      CleanString.of("abc").value shouldBe "abc"
      CleanString.of("a").value shouldBe "a"
      CleanString.of("xx xx").value shouldBe "xx xx"
    }

    "succeed with leading/trailing whitespace and non-blank strings" in {

      CleanString.of(" abc").value shouldBe "abc"
      CleanString.of(" abc").value shouldBe "abc"
      CleanString.of(" abc ").value shouldBe "abc"
      CleanString.of(" \t xx xx \n ").value shouldBe "xx xx"
    }

    "fail for an empty string" in {
      an[IllegalArgumentException] should be thrownBy CleanString.of("")
    }

    "fail for a blank string" in {
      an[IllegalArgumentException] should be thrownBy CleanString.of(" ")
      an[IllegalArgumentException] should be thrownBy CleanString.of("    ")
      an[IllegalArgumentException] should be thrownBy CleanString.of("\n")
      an[IllegalArgumentException] should be thrownBy CleanString.of("\t")
      an[IllegalArgumentException] should be thrownBy CleanString.of("   \t  \n ")
    }
  }

  "Sorting CleanStrings" should {

    "order by the underlying value" in {

      Seq(CleanString("abc"), CleanString("aaa"), CleanString("abb")).sorted shouldBe
        Seq(CleanString("aaa"), CleanString("abb"), CleanString("abc"))
    }
  }
}
