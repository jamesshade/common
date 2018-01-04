package org.shade.common.parsers


trait Parser[T] {

  def parse(str: String): Option[T]

  object Parsed {
    def unapply(s: String): Option[T] = parse(s)
  }
}
