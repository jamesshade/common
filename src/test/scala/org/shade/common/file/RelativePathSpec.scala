package org.shade.common.file

import java.io.File
import java.nio.file.Paths

import org.scalatest.{Matchers, WordSpec}


class RelativePathSpec extends WordSpec with Matchers {

  private def rp(path: String) = RelativePath(Paths.get(path))

  "empty" should {

    "be an empty relative path" in {
      RelativePath.empty shouldBe rp("")
    }
  }

  "Constructing a RelativePath" should {

    "fail if the underlying path is absolute" in {
      an[IllegalArgumentException] should be thrownBy rp("/abc")
      an[IllegalArgumentException] should be thrownBy rp("/abc/def")
      an[IllegalArgumentException] should be thrownBy rp("/")
    }

    "succeed if the path is relative" in {
      rp("abc").value.isAbsolute shouldBe false
      rp("abc/def").value.isAbsolute shouldBe false
      rp("").value.isAbsolute shouldBe false
    }
  }

  "concatenating two relative paths" should {

    "return the correct combination" in {
      rp("a") ++ rp("b") shouldBe rp("a/b")
      rp("a") ++ rp("") shouldBe rp("a")
      rp("") ++ rp("b") shouldBe rp("b")
      rp("") ++ rp("") shouldBe rp("")
      rp("x") ++ rp("x") shouldBe rp("x/x")
    }
  }

  "concatenating a Filename to a relative path" should {

    "add the filename correctly" in {
      rp("a") ++ Filename("b") shouldBe rp("a/b")
      rp("") ++ Filename("b") shouldBe rp("b")
      rp("a/b/c/d") ++ Filename("e.txt") shouldBe rp("a/b/c/d/e.txt")
    }
  }

  "file" should {

    "return the expected File" in {
      rp("a").file shouldBe new File("a")
      rp("").file shouldBe new File("")
      rp("a/b/c/d").file shouldBe new File("a/b/c/d")
    }
  }

  "getName" should {

    "return the final element of the path" in {
      rp("a").getName shouldBe Some(Filename("a"))
      rp("a/b/c").getName shouldBe Some(Filename("c"))
      rp("a/b/c/d.txt").getName shouldBe Some(Filename("d.txt"))
    }

    "return None if there is no filename element" in {
      rp("").getName shouldBe None
    }
  }

  "name" should {

    "return the final element of the path" in {
      rp("a").name shouldBe Filename("a")
      rp("a/b/c").name shouldBe Filename("c")
      rp("a/b/c/d.txt").name shouldBe Filename("d.txt")
    }

    "fail if there is no filename element" in {
      an [IllegalStateException] should be thrownBy rp("").name
    }
  }

  "isEmpty" should {

    "return true if the path is empty" in {
      RelativePath.empty.isEmpty shouldBe true
      rp("").isEmpty shouldBe true
    }

    "return false otherwise" in {
      rp("a").isEmpty shouldBe false
      rp("abc").isEmpty shouldBe false
      rp("a/b/c").isEmpty shouldBe false
    }
  }

  "nonEmpty" should {

    "return true if the path is non-empty" in {
      rp("a").nonEmpty shouldBe true
      rp("abc").nonEmpty shouldBe true
      rp("a/b/c").nonEmpty shouldBe true
    }

    "return false otherwise" in {
      RelativePath.empty.nonEmpty shouldBe false
      rp("").nonEmpty shouldBe false
    }
  }

  "parent" should {

    "return the parent of the current path" in {
      rp("a/b/c").parent shouldBe rp("a/b")
      rp("a/b").parent shouldBe rp("a")
    }

    "return an empty path if there is only one element" in {
      rp("a").parent shouldBe RelativePath.empty
    }

    "fail if there are no elements" in {
      an [IllegalStateException] should be thrownBy RelativePath.empty.parent
    }
  }
}
