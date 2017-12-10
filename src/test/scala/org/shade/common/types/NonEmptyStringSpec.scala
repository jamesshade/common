package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class NonEmptyStringSpec extends WordSpec with Matchers {

  "Construction" should {

    "succeed for non-empty strings" in {

      NonEmptyString("abc").value shouldBe "abc"
      NonEmptyString("ab").value shouldBe "ab"
      NonEmptyString("a").value shouldBe "a"
    }

    "allow leading/trailing whitespace" in {
      NonEmptyString(" ").value shouldBe " "
      NonEmptyString("  ").value shouldBe "  "
      NonEmptyString(" \txx xx\n ").value shouldBe " \txx xx\n "
    }

    "fail for an empty string" in {
      an[IllegalArgumentException] should be thrownBy NonEmptyString("")
    }
  }

  "Sorting NonEmptyStrings" should {

    "order by the underlying value" in {

      Seq(NonEmptyString("abc"), NonEmptyString("aaa"), NonEmptyString("abb")).sorted shouldBe
        Seq(NonEmptyString("aaa"), NonEmptyString("abb"), NonEmptyString("abc"))
    }
  }
}
