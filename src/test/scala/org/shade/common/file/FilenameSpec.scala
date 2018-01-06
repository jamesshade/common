package org.shade.common.file

import org.scalatest.{Matchers, WordSpec}


class FilenameSpec extends WordSpec with Matchers {

  "Constructing a filename" should {

    "file if the filename is empty" in {
      an [IllegalArgumentException] should be thrownBy Filename("")
    }

    "fail if the filename contains a slash" in {
      an [IllegalArgumentException] should be thrownBy Filename("/abc.txt")
      an [IllegalArgumentException] should be thrownBy Filename("dir/abc.txt")
      an [IllegalArgumentException] should be thrownBy Filename("abc.txt/")
    }

    "succeed if the filename is non-empty and contains no slashes" in {
      Filename("abc.txt").value shouldBe "abc.txt"
      Filename("a").value shouldBe "a"
    }

    "permit whitespace" in {
      Filename("  def.txt  ").value shouldBe "  def.txt  "
      Filename(" ").value shouldBe " "
    }
  }

  "Sorting filenames" should {

    "succeed for lists with no duplicates" in {
      Seq(Filename("c"), Filename("a"), Filename("b")).sorted.map(_.value) shouldBe Seq("a", "b", "c")
    }

    "succeed for lists with duplicates" in {
      Seq(Filename("c"), Filename("b"), Filename("a"), Filename("c"), Filename("a"), Filename("b")).sorted.map(_.value) shouldBe
        Seq("a", "a", "b", "b", "c", "c")
    }
  }
}
