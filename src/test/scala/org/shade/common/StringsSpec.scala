package org.shade.common

import org.scalatest.{Matchers, WordSpec}


class StringsSpec extends WordSpec with Matchers {

  import Strings._

  "shorten" should {

    "throw an IllegalStateException if the length of the ellipses is the same as the max final string length" in {
      an [IllegalArgumentException] should be thrownBy "abcde".shorten(3, "...")
      an [IllegalArgumentException] should be thrownBy "abcde".shorten(0, "")
      an [IllegalArgumentException] should be thrownBy "abcde".shorten(5, "123456")
    }

    "shorten a string that's too long, appending ellipses" in {
      "abcde".shorten(4, "...") shouldBe "a..."
      "abcdefgh".shorten(4, "...") shouldBe "a..."
      "abcdefghijklm".shorten(10, "??") shouldBe "abcdefgh??"
      "abcdefghijklm".shorten(10, "") shouldBe "abcdefghij"
    }

    "don't change a string that already fits" in {
      "abcd".shorten(4, "...") shouldBe "abcd"
      "abc".shorten(4, "...") shouldBe "abc"
      "abcdefghij".shorten(10, "??") shouldBe "abcdefghij"
      "abcdefghij".shorten(10, "") shouldBe "abcdefghij"
    }

    "default to ellipses '...'" in {
      an[IllegalArgumentException] should be thrownBy "abcde".shorten(3)
      "abcdefghij".shorten(8) shouldBe "abcde..."
    }
  }

  "chomp" should {

    "remove the supplied string from the wrapped string" in {
      "abc".chomp("") shouldBe "abc"
      "abc".chomp("c") shouldBe "ab"
      "abc".chomp("bc") shouldBe "a"
      "abc".chomp("abc") shouldBe ""
      "this is a long string\n".chomp("\n") shouldBe "this is a long string"
      "this is a long string\n".chomp("ng\n") shouldBe "this is a long stri"
    }

    "do nothing if the supplied string is not at the end of the wrapped string" in {
      "abc".chomp("d") shouldBe "abc"
      "abc".chomp("b") shouldBe "abc"
      "abc".chomp("a") shouldBe "abc"
      "abc".chomp("ab") shouldBe "abc"
      "this is a long string\n".chomp("ng") shouldBe "this is a long string\n"
      "".chomp("") shouldBe ""
    }
  }

  "firstLower" should {

    "convert the first character to lower case" in {
      "ABC".firstLower shouldBe "aBC"
      "XYZ ABC".firstLower shouldBe "xYZ ABC"
    }

    "do nothing if the first character is not a letter" in {
      "123".firstLower shouldBe "123"
      "$BC".firstLower shouldBe "$BC"
      " ABC".firstLower shouldBe " ABC"
    }

    "do nothing if the first character is already lower-case" in {
      "abc".firstLower shouldBe "abc"
      "aBC DEF".firstLower shouldBe "aBC DEF"
    }

    "do nothing if the string is empty" in {
      "".firstLower shouldBe ""
    }
  }

  "firstUpper" should {

    "convert the first character to upper case" in {
      "abc".firstUpper shouldBe "Abc"
      "xyz abc".firstUpper shouldBe "Xyz abc"
    }

    "do nothing if the first character is not a letter" in {
      "123".firstUpper shouldBe "123"
      "$bc".firstUpper shouldBe "$bc"
      " abc".firstUpper shouldBe " abc"
    }

    "do nothing if the first character is already upper-case" in {
      "ABC".firstUpper shouldBe "ABC"
      "Abc def".firstUpper shouldBe "Abc def"
    }

    "do nothing if the string is empty" in {
      "".firstUpper shouldBe ""
    }
  }
}
