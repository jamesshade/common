package org.shade.common.types

import org.scalatest.{Matchers, WordSpec}


class EnumCompanionSpec extends WordSpec with Matchers {

  import EnumCompanionSpec._

  "An EnumCompanion backed by a wrapped int" should {

    "provide an all list returning all values" in {
      Animal.all shouldBe Seq(Cat, Dog, Rabbit, Horse, Antelope)
    }

    "provide an apply method taking an int" that {

      "constructs for a valid value" in {
        Animal(8) shouldBe Rabbit
        Animal(4) shouldBe Antelope
      }

      "throws an IllegalArgumentException for an invalid value" in {
        val e = the [IllegalArgumentException] thrownBy Animal(0)
        e.getMessage should endWith ("Invalid Animal value [0]")
      }
    }

    "provide an of method taking an Int" that {

      "constructs for a valid value" in {
        Animal.of(9) shouldBe Some(Horse)
        Animal.of(3) shouldBe Some(Dog)
      }

      "returns None for an invalid value" in {
        Animal.of(15) shouldBe None
      }
    }

    "provide a parse method" that {

      "constructs for a valid value" in {
        Animal.parse("6") shouldBe Some(Cat)
      }

      "returns None for an invalid value" in {
        Animal.parse("blah") shouldBe None
        Animal.parse("Cat") shouldBe None
        Animal.parse("Dog") shouldBe None
        Animal.parse("12") shouldBe None
      }
    }

    "provide an implicit Ordering implementation the orders based on the all collection" in {

      Seq[Animal](Antelope, Dog, Horse, Rabbit, Dog, Cat, Rabbit).sorted shouldBe
        Seq(Cat, Dog, Dog, Rabbit, Rabbit, Horse, Antelope)
    }

    "throw an exception when ordering for any values not in the collection" in {

      val e = the [IllegalArgumentException] thrownBy Seq[Animal](Antelope, Dog, Horse, Unicorn, Dog, Cat, Rabbit).sorted
      e.getMessage shouldBe "Not in Animal enum: Unicorn:5"
    }
  }
}


private object EnumCompanionSpec {

  sealed trait Animal extends WrappedInt

  case object Cat extends Animal {
    override val value = 6
  }

  case object Dog extends Animal {
    override val value = 3
  }

  case object Rabbit extends Animal {
    override val value = 8
  }

  case object Horse extends Animal {
    override val value = 9
  }

  case object Antelope extends Animal {
    override val value = 4
  }

  case object Unicorn extends Animal {
    override val value = 5
  }

  object Animal extends EnumCompanion[Int, Animal] {
    override val all: Seq[Animal] = Seq(Cat, Dog, Rabbit, Horse, Antelope)
  }
}