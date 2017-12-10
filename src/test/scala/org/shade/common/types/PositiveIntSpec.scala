package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class PositiveIntSpec extends WordSpec with Matchers {

  "Construction" should {

    "succeed for positive ints" in {

      (1 to 100).foreach { i =>
        PositiveInt(i).value shouldBe i
      }

      (Int.MaxValue - 100 to Int.MaxValue).foreach { i =>
        PositiveInt(i).value shouldBe i
      }
    }

    "fail for zero" in {
      an[IllegalArgumentException] should be thrownBy PositiveInt(0)
    }

    "fail for anything else" in {

      (Int.MinValue to Int.MinValue + 100).foreach { i =>
        an[IllegalArgumentException] should be thrownBy PositiveInt(i)
      }

      (-100 to -1).foreach { i =>
        an[IllegalArgumentException] should be thrownBy PositiveInt(i)
      }
    }
  }
}
