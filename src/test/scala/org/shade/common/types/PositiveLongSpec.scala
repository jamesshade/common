package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class PositiveLongSpec extends WordSpec with Matchers {

  "Construction" should {

    "succeed for positive longs" in {

      (1L to 100L).foreach { i =>
        PositiveLong(i).value shouldBe i
      }

      (Long.MaxValue - 100L to Long.MaxValue).foreach { i =>
        PositiveLong(i).value shouldBe i
      }
    }

    "fail for zero" in {
      an[IllegalArgumentException] should be thrownBy PositiveLong(0L)
    }

    "fail for anything else" in {

      (Long.MinValue to Long.MinValue + 100L).foreach { i =>
        an[IllegalArgumentException] should be thrownBy PositiveLong(i)
      }

      (-100L to -1L).foreach { i =>
        an[IllegalArgumentException] should be thrownBy PositiveLong(i)
      }
    }
  }
}
