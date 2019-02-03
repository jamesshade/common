package org.shade.common.types


object Implicits {

  implicit class StringDecorator(str: String) {

    lazy val cleanSafely: Option[CleanString] = {

      str.trim match {
        case "" => None
        case other => Some(CleanString(other))
      }
    }

    lazy val clean: CleanString = CleanString.of(str)
  }

  implicit class OptionalStringDecorator(maybeStr: Option[String]) {

    lazy val clean: Option[CleanString] = maybeStr.flatMap(_.cleanSafely)
  }
}
