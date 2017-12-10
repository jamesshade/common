package org.shade.common.types


case class PositiveInt(value: Int) extends WrappedInt {
  require(value >= 1, s"Value must be >= 1, got: $value")
}


case class NonNegativeInt(value: Int) extends WrappedInt {
  require(value >= 0, s"Value must be >= 0, got: $value")
}


case class PositiveLong(value: Long) extends WrappedLong {
  require(value >= 1L, s"Value must be >= 1, got: $value")
}


case class NonNegativeLong(value: Long) extends WrappedLong {
  require(value >= 0L, s"Value must be >= 0, got: $value")
}


case class NonEmptyString(value: String) extends WrappedString with Ordered[NonEmptyString] {
  require(value.nonEmpty, "Value must not be empty")

  override def compare(that: NonEmptyString) = value.compare(that.value)
}


case class CleanString(value: String) extends WrappedString with Ordered[CleanString] {

  require(value.trim == value, "Value must not have leading/trailing whitespace")
  require(value.nonEmpty, "Value must not be empty")

  override def compare(that: CleanString) = value.compare(that.value)
}

object CleanString {

  def of(rawValue: String): CleanString = {

    require(rawValue.nonEmpty, "Value must not be empty")

    val value = rawValue.trim

    require(value.nonEmpty, "Value must not be blank")

    CleanString(value)
  }
}
