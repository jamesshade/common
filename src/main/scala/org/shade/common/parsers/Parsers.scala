package org.shade.common.parsers

import java.time.Instant

import scala.util.Try


trait Parsers {

  implicit class ParseDecorator(str: String) {

    def parseInt: Option[Int] = Try(str.trim.toInt).toOption

    def parseLong: Option[Long] = Try(str.trim.toLong).toOption

    def parseInstant: Option[Instant] = Try(Instant.parse(str.trim)).toOption
  }
}

object Parsers extends Parsers