package org.shade.common


object Strings {

  implicit class StringDecorator(str: String) {

    private val len = str.length

    def shorten(maxLengthIncludingEllipses: Int, ellipses: String = "..."): String = {

      require(maxLengthIncludingEllipses > ellipses.length,
        s"maxLengthIncludingEllipses ($maxLengthIncludingEllipses) must be > length of ellipses '$ellipses' (${ellipses.length}"
      )

      if (len <= maxLengthIncludingEllipses) str else {

        val trimAt = maxLengthIncludingEllipses - ellipses.length

        if (str.length < trimAt) str else s"${str.substring(0, trimAt)}$ellipses"
      }
    }

    def chomp(trailingString: String): String = {
      if (str.endsWith(trailingString))
        str.substring(0, str.length - trailingString.length)
      else
        str
    }

    def firstLower: String = str.headOption match {
      case Some(c) if c.isLower => str
      case Some(c) => c.toLower +: str.tail
      case None => str
    }

    def firstUpper: String = str.headOption match {
      case Some(c) if c.isUpper => str
      case Some(c) => c.toUpper +: str.tail
      case None => str
    }
  }
}
