package org.shade.common.types

import org.shade.common.Strings._


trait EnumCompanion[T, W <: Wrapped[T]] {

  val all: Seq[W]

  protected lazy final val name: String = getClass.getSimpleName match {
    case s if s.endsWith("$") => s.substring(0, s.length - 1)
    case s => s
  }

  final def apply(underlying: T): W = of(underlying).getOrElse {
    throw new IllegalArgumentException(s"Invalid $name value [${safely(underlying.toString)}]")
  }

  final def of(underlying: T): Option[W] =  all.find(_.value == underlying)

  final def parse(str: String): Option[W] = all.find(_.toString == str)

  implicit val ordering: Ordering[W] =
    (left: W, right: W) => left.index.compareTo(right.index)

  protected final def safely(str: String): String = str.shorten(truncateForErrorsAt)

  private lazy val indexed: Map[W, Int] = all.zipWithIndex.toMap

  private lazy val truncateForErrorsAt = all.toString.map(_.toString.length).max + 10

  private implicit class ItemDecorator(value: W) {

    lazy val errorString = value match {
      case p: Product if p.productPrefix != value.toString => s"${p.productPrefix}:$value"
      case _ => value.toString
    }

    lazy val index: Int = indexed.getOrElse(value,
      throw new IllegalArgumentException(s"Not in $name enum: $errorString")
    )
  }
}