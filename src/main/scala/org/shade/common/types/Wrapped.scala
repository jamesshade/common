package org.shade.common.types

import java.util.UUID


trait Wrapped[T] {
  val value: T
  override final lazy val toString = value.toString
}


trait WrappedUuid extends Wrapped[UUID]


trait WrappedString extends Wrapped[String]


trait WrappedInt extends Wrapped[Int]


trait WrappedLong extends Wrapped[Long]
