package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class NonNegativeIntegerSpec extends WordSpec with Matchers {

  "Construction" should {

    "succeed for positive ints" in {

      (1 to 100).foreach { i =>
        NonNegativeInt(i).value shouldBe i
      }

      (Int.MaxValue - 100 to Int.MaxValue).foreach { i =>
        NonNegativeInt(i).value shouldBe i
      }
    }

    "succeed for zero" in {
      NonNegativeInt(0).value shouldBe 0
    }

    "fail for anything else" in {

      (Int.MinValue to Int.MinValue + 100).foreach { i =>
        an[IllegalArgumentException] should be thrownBy NonNegativeInt(i)
      }

      (-100 to -1).foreach { i =>
        an[IllegalArgumentException] should be thrownBy NonNegativeInt(i)
      }
    }
  }
}
