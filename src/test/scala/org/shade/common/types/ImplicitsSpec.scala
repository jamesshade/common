package org.shade.common.types

import Implicits._
import org.scalatest.{Matchers, WordSpec}


class ImplicitsSpec extends WordSpec with Matchers {

  "StringDecorator.cleanSafely" should {

    "return None if the string is empty" in {
      "".cleanSafely shouldBe None
    }

    "return None if the string trims to an empty string" in {
      " ".cleanSafely shouldBe None
      "    ".cleanSafely shouldBe None
      "   \t   \n  ".cleanSafely shouldBe None
    }

    "return the clean string if it's clean already" in {
      "some string".cleanSafely shouldBe Some(CleanString("some string"))
    }

    "clean and return the string if it is not blank" in {
      "   \t  \n  some \t  string \n  ".cleanSafely shouldBe Some(CleanString("some \t  string"))
    }
  }

  "StringDecorator.clean" should {

    "throw an IllegalArgumentException if the string is empty" in {
      an [IllegalArgumentException] should be thrownBy "".clean
    }

    "throw an IllegalArgumentException if the string trims to an empty string" in {
      an [IllegalArgumentException] should be thrownBy " ".clean
      an [IllegalArgumentException] should be thrownBy "    ".clean
      an [IllegalArgumentException] should be thrownBy "   \t   \n  ".clean
    }

    "return the clean string if it's clean already" in {
      "some string".clean shouldBe CleanString("some string")
    }

    "clean and return the string if it is not blank" in {
      "   \t  \n  some \t  string \n  ".clean shouldBe CleanString("some \t  string")
    }
  }

  "OptionalStringDecorator.clean" should {

    "return None if the input is None" in {
      (None: Option[String]).clean shouldBe None
    }

    "return None if the string is empty" in {
      Some("").clean shouldBe None
    }

    "return None if the string trims to an empty string" in {
      Some(" ").clean shouldBe None
      Some("    ").clean shouldBe None
      Some("   \t   \n  ").clean shouldBe None
    }

    "return the clean string if it's clean already" in {
      Some("some string").clean shouldBe Some(CleanString("some string"))
    }

    "clean and return the string if it is not blank" in {
      Some("   \t  \n  some \t  string \n  ").clean shouldBe Some(CleanString("some \t  string"))
    }
  }
}
