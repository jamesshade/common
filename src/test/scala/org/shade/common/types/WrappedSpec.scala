package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class WrappedSpec extends WordSpec with Matchers {

  "toString" should {

    "call toString on the wrappedValue" in {

      case class MyClass(x: Int) {
        override val toString = "some string"
      }

      case class MyWrapper(value: MyClass) extends Wrapped[MyClass]

      val wrappedValue = MyWrapper(MyClass(6))

      wrappedValue.toString shouldBe "some string"
    }
  }
}
