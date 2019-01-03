package org.shade.common.file

import java.io.File
import java.nio.file.Paths

import org.scalatest.{Matchers, WordSpec}


class AbsolutePathSpec extends WordSpec with Matchers {

  private def ap(path: String) = AbsolutePath(Paths.get(path))

  private def of(path: String) = AbsolutePath.of(Paths.get(path))

  private def rp(path: String) = RelativePath(Paths.get(path))

  "root" should {

    "represent the root path" in {
      AbsolutePath.root shouldBe ap("/")
    }
  }

  "of" should {

    "return Left with an error message if the path is relative" in {
      of("abc") shouldBe Left("Expected an absolute path, got: abc")
      of("abc/def") shouldBe Left("Expected an absolute path, got: abc/def")
      of("") shouldBe Left("Expected an absolute path, got an empty path")
    }

    "return the path if it is absolute" in {
      of("/abc") shouldBe Right(AbsolutePath(Paths.get("/abc")))
      of("/abc/def") shouldBe Right(AbsolutePath(Paths.get("/abc/def")))
      of("/") shouldBe Right(AbsolutePath(Paths.get("/")))
    }
  }

  "Constructing an AbsolutePath" should {

    "fail if the underlying path is relative" in {
      an[IllegalArgumentException] should be thrownBy ap("abc")
      an[IllegalArgumentException] should be thrownBy ap("abc/def")
      an[IllegalArgumentException] should be thrownBy ap("")
    }

    "succeed if the path is absolute" in {
      ap("/abc").value.isAbsolute shouldBe true
      ap("/abc/def").value.isAbsolute shouldBe true
      ap("/").value.isAbsolute shouldBe true
    }
  }

  "concatenating a relative path to an absolute path" should {

    "return the correct absolute path" in {
      ap("/a") ++ rp("b") shouldBe ap("/a/b")
      ap("/a") ++ rp("") shouldBe ap("/a")
      ap("/a/b/c") ++ rp("b") shouldBe ap("/a/b/c/b")
      ap("/") ++ rp("") shouldBe ap("/")
      ap("/") ++ rp("x") shouldBe ap("/x")
    }
  }

  "concatenating a Filename to an absolute path" should {

    "add the filename correctly" in {
      ap("/a") ++ Filename("b") shouldBe ap("/a/b")
      ap("/") ++ Filename("b") shouldBe ap("/b")
      ap("/a/b/c/d") ++ Filename("e.txt") shouldBe ap("/a/b/c/d/e.txt")
    }
  }

  "file" should {

    "return the expected File" in {
      ap("/a").file shouldBe new File("/a")
      ap("/").file shouldBe new File("/")
      ap("/a/b/c/d").file shouldBe new File("/a/b/c/d")
    }
  }

  "getName" should {

    "return the final element of the path" in {
      ap("/a").getName shouldBe Some(Filename("a"))
      ap("/a/b/c").getName shouldBe Some(Filename("c"))
      ap("/a/b/c/d.txt").getName shouldBe Some(Filename("d.txt"))
    }

    "return None if there is no filename element" in {
      ap("/").getName shouldBe None
    }
  }

  "name" should {

    "return the final element of the path" in {
      ap("/a").name shouldBe Filename("a")
      ap("/a/b/c").name shouldBe Filename("c")
      ap("/a/b/c/d.txt").name shouldBe Filename("d.txt")
    }

    "fail if there is no final element" in {
      an[IllegalStateException] should be thrownBy ap("/").name
    }
  }

  "isRoot" should {

    "return true if the path is the root path" in {
      AbsolutePath.root.isRoot shouldBe true
      ap("/").isRoot shouldBe true
    }

    "return false otherwise" in {
      ap("/a").isRoot shouldBe false
      ap("/abc").isRoot shouldBe false
      ap("/a/b/c").isRoot shouldBe false
    }
  }

  "nonRoot" should {

    "return true if the path is non-empty" in {
      ap("/a").nonRoot shouldBe true
      ap("/abc").nonRoot shouldBe true
      ap("/a/b/c").nonRoot shouldBe true
    }

    "return false otherwise" in {
      AbsolutePath.root.nonRoot shouldBe false
      ap("/").nonRoot shouldBe false
    }
  }

  "parent" should {

    "return the parent of the current path" in {
      ap("/a/b/c").parent shouldBe ap("/a/b")
      ap("/a/b").parent shouldBe ap("/a")
    }

    "return the root path if there is only one element" in {
      ap("/a").parent shouldBe AbsolutePath.root
    }

    "fail if we are at the root" in {
      an[IllegalStateException] should be thrownBy AbsolutePath.root.parent
    }
  }

  "relativise, passed a Path" should {

    "return the relative path of that directory to this" in {
      ap("/a/b/c").relativise(Paths.get("/a/b/c/d/e")) shouldBe rp("d/e")
      ap("/a/b/c").relativise(Paths.get("/a/b/c")) shouldBe RelativePath.empty
      ap("/").relativise(Paths.get("/c")) shouldBe rp("c")
      ap("/a/b/c").relativise(Paths.get("/d/e/f")) shouldBe rp("../../../d/e/f")
      ap("/a/b/c/d").relativise(Paths.get("/a/b/g/e")) shouldBe rp("../../g/e")
    }

    "fail if the other path is not absolute" in {
      an[IllegalArgumentException] should be thrownBy ap("/a/b/c").relativise(Paths.get("a/b/c/d/e"))
    }
  }

  "relativise, passed a File" should {

    "return the relative path of that directory to this" in {
      ap("/a/b/c").relativise(new File("/a/b/c/d/e")) shouldBe rp("d/e")
      ap("/a/b/c").relativise(new File("/a/b/c")) shouldBe RelativePath.empty
      ap("/").relativise(new File("/c")) shouldBe rp("c")
      ap("/a/b/c").relativise(new File("/d/e/f")) shouldBe rp("../../../d/e/f")
      ap("/a/b/c/d").relativise(new File("/a/b/g/e")) shouldBe rp("../../g/e")
    }

    "fail if the other path is not absolute" in {
      an[IllegalArgumentException] should be thrownBy ap("/a/b/c").relativise(new File("a/b/c/d/e"))
    }
  }

  "relativise, passed an AbsolutePath" should {

    "return the relative path of that directory to this" in {
      ap("/a/b/c").relativise(ap("/a/b/c/d/e")) shouldBe rp("d/e")
      ap("/a/b/c").relativise(ap("/a/b/c")) shouldBe RelativePath.empty
      ap("/").relativise(ap("/c")) shouldBe rp("c")
      ap("/a/b/c").relativise(ap("/d/e/f")) shouldBe rp("../../../d/e/f")
      ap("/a/b/c/d").relativise(ap("/a/b/g/e")) shouldBe rp("../../g/e")
    }
  }
}
