package org.shade.common.parsers

import java.time.Instant

import org.scalatest.{Matchers, WordSpec}


class ParsersSpec extends WordSpec with Matchers {

  import Parsers._

  "ParseDecorator" when {

    "parsing an Int" should {

      "return Some(value) if the string contains a valid Int" in {
        "6".parseInt shouldBe Some(6)
        "0".parseInt shouldBe Some(0)
        "-6".parseInt shouldBe Some(-6)
        s"${Int.MinValue}".parseInt shouldBe Some(Int.MinValue)
        s"${Int.MaxValue}".parseInt shouldBe Some(Int.MaxValue)
      }

      "cope with whitespace" in {
        "  45  ".parseInt shouldBe Some(45)
        "45  ".parseInt shouldBe Some(45)
        "  45".parseInt shouldBe Some(45)
      }

      "return None otherwise" in {
        "".parseInt shouldBe None
        "abc".parseInt shouldBe None
        "1.5".parseInt shouldBe None
        (null: String).parseInt shouldBe None
        s"${Int.MinValue.toLong - 1L}".parseInt shouldBe None
        s"${Int.MaxValue.toLong + 1L}".parseInt shouldBe None
      }
    }

    "parsing a Long" should {

      "return Some(value) if the string contains a valid Long" in {
        "6".parseLong shouldBe Some(6L)
        "0".parseLong shouldBe Some(0L)
        "-6".parseLong shouldBe Some(-6L)
        s"${Long.MinValue}".parseLong shouldBe Some(Long.MinValue)
        s"${Long.MaxValue}".parseLong shouldBe Some(Long.MaxValue)
      }

      "cope with whitespace" in {
        "  45  ".parseLong shouldBe Some(45L)
        "45  ".parseLong shouldBe Some(45L)
        "  45".parseLong shouldBe Some(45L)
      }

      "return None otherwise" in {
        "".parseLong shouldBe None
        "abc".parseLong shouldBe None
        "1.5".parseLong shouldBe None
        "1L".parseLong shouldBe None
        (null: String).parseLong shouldBe None
        s"${BigDecimal(Long.MinValue) - BigDecimal(1)}".parseLong shouldBe None
        s"${BigDecimal(Long.MaxValue) + BigDecimal(1)}".parseLong shouldBe None
      }
    }

    "parsing an Instant" should {

      "return Some(value) if the string contains a valid Instant" in {
        "2018-01-04T14:19:28Z".parseInstant shouldBe Some(Instant.ofEpochSecond(1515075568L))
        "2018-01-04T14:19:28.837Z".parseInstant shouldBe Some(Instant.ofEpochSecond(1515075568L).plusNanos(837000000L))
        "2018-01-04T14:19:28.837345Z".parseInstant shouldBe Some(Instant.ofEpochSecond(1515075568L).plusNanos(837345000L))
        "2018-01-04T14:19:28.837543234Z".parseInstant shouldBe Some(Instant.ofEpochSecond(1515075568L).plusNanos(837543234L))
      }

      "cope with whitespace" in {
        "  2018-01-04T14:19:28Z  ".parseInstant shouldBe Some(Instant.ofEpochSecond(1515075568L))
      }

      "return None otherwise" in {
        "2018-01-04T14:19:28.837".parseInstant shouldBe None
        "2018-01-04 14:19:28.837Z".parseInstant shouldBe None
        "2018-01-04X14:19:28.837Z".parseInstant shouldBe None
        "2018/01/04T14:19:28.837Z".parseInstant shouldBe None
        "2018-02-29T14:19:28.837Z".parseInstant shouldBe None
        "other".parseInstant shouldBe None
      }
    }
  }
}
