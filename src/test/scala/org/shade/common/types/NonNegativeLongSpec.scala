package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class NonNegativeLongSpec extends WordSpec with Matchers {

  "Construction" should {

    "succeed for positive longs" in {

      (1L to 100L).foreach { i =>
        NonNegativeLong(i).value shouldBe i
      }

      (Long.MaxValue - 100L to Long.MaxValue).foreach { i =>
        NonNegativeLong(i).value shouldBe i
      }
    }

    "succeed for zero" in {
      NonNegativeLong(0L).value shouldBe 0L
    }

    "fail for anything else" in {

      (Long.MinValue to Long.MinValue + 100L).foreach { i =>
        an[IllegalArgumentException] should be thrownBy NonNegativeLong(i)
      }

      (-100L to -1L).foreach { i =>
        an[IllegalArgumentException] should be thrownBy NonNegativeLong(i)
      }
    }
  }
}
