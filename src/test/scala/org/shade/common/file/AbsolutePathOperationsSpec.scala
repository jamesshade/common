package org.shade.common.file

import java.io.{File, IOException}
import java.nio.file.Files
import java.time.Instant
import java.time.temporal.ChronoUnit

import org.apache.commons.io.FileUtils
import org.scalatest.{Matchers, WordSpec}
import org.shade.common.types.NonNegativeLong


class AbsolutePathOperationsSpec extends WordSpec with Matchers {

  import PathOperations._

  "exists" should {

    "return true if the path exists and is a regular file" in testDir { dir =>

      val path = dir.createFile()

      path.exists shouldBe true
    }

    "return true if the path exists and is a directory" in testDir { dir =>

      val path = dir.createDirectory()

      path.exists shouldBe true
    }

    "return true if the path exists and is a valid symbolic link" in testDir { dir =>

      val target = dir.createFile()
      val path = dir.createSymbolicLinkTo(target)

      path.exists shouldBe true
    }

    "return true if the path is a symbolic link, even if the link destination is missing" in testDir { dir =>

      val path = dir.createSymbolicLinkTo(dir.missingPath)

      path.exists shouldBe true
    }

    "return false if the path does not exist" in testDir { dir =>

      dir.missingPath.exists shouldBe false
    }
  }

  "isFile" should {

    "return true if the path exists and is a regular file" in testDir { dir =>

      val path = dir.createFile()

      path.isFile shouldBe true
    }

    "return false if the path exists but is a directory" in testDir { dir =>

      val path = dir.createDirectory()

      path.isFile shouldBe false
    }

    "return false if the paths exists but is a symbolic link (to a file)" in testDir { dir =>

      val path = dir.createSymbolicLinkTo(dir.createFile())

      path.isFile shouldBe false
    }

    "return false if the path does not exist" in testDir { dir =>

      dir.missingPath.isFile shouldBe false
    }
  }

  "isDirectory" should {

    "return false if the path exists but is a regular file" in testDir { dir =>

      val path = dir.createFile()

      path.isDirectory shouldBe false
    }

    "return true if the file exists and is a directory" in testDir { dir =>

      val path = dir.createDirectory()

      path.isDirectory shouldBe true
    }

    "return false if the paths exists but is a symbolic link (to a directory)" in testDir { dir =>

      val path = dir.createSymbolicLinkTo(dir.createDirectory())

      path.isDirectory shouldBe false
    }

    "return false if the path does not exist" in testDir { dir =>

      dir.missingPath.isDirectory shouldBe false
    }
  }

  "isSymbolicLink" should {

    "return false if the path exists but is a regular file" in testDir { dir =>

      val path = dir.createFile()

      path.isSymbolicLink shouldBe false
    }

    "return false if the path exists but is a directory" in testDir { dir =>

      val path = dir.createDirectory()

      path.isSymbolicLink shouldBe false
    }

    "return false if the path does not exist" in testDir { dir =>

      dir.missingPath.isSymbolicLink shouldBe false
    }

    "return true if the path exists and is a valid symbolic link" in testDir { dir =>

      val path = dir.createSymbolicLinkTo(dir.createFile())

      path.isSymbolicLink shouldBe true
    }

    "return true if the path exists and is an invalid symbolic link" in testDir { dir =>

      val path = dir.createSymbolicLinkTo(dir.missingPath)

      path.isSymbolicLink shouldBe true
    }
  }

  "size" should {

    "return the size of the file if it exists and is a regular file" in testDir { dir =>

      val path = dir.createFile()

      path.size shouldBe NonNegativeLong(dir.fileBytes.length)
    }

    "return zero if the file exists, but is empty" in testDir { dir =>

      val path = dir.createFile(Array.empty[Byte])

      path.size shouldBe NonNegativeLong(0)
    }

    "throw an IO exception if the path exists but is a directory" in testDir { dir =>

      val path = dir.createDirectory()

      an[IOException] should be thrownBy path.size
    }

    "throw an IO exception if the path exists but is a symbolic link" in testDir { dir =>

      val path = dir.createSymbolicLinkTo(dir.createFile())

      an[IOException] should be thrownBy path.size
    }

    "throw an IO exception if the path does not exist" in testDir { dir =>

      val path = dir.testRoot ++ Filename("not-there")

      an[IOException] should be thrownBy path.size
    }
  }

  "lastModified" should {

    def doTest(create: => AbsolutePath) = {

      val before = Instant.now.truncatedTo(ChronoUnit.SECONDS)
      val path = create
      val after = Instant.now.truncatedTo(ChronoUnit.SECONDS).plusSeconds(1)

      val lastModified = path.lastModified

      lastModified should be >= before
      lastModified should be < after
    }

    "return the last modified date of a file" in testDir { dir =>
      doTest(dir.createFile())
    }

    "return the last modified date of a directory" in testDir { dir =>
      doTest(dir.createFile())
    }

    "return the last modified date of a symbolic link" in testDir { dir =>
      doTest(dir.createSymbolicLinkTo(dir.missingPath))
    }

    "throw an IOException if the file is missing" in testDir { dir =>
      an [IOException] should be thrownBy dir.missingPath.lastModified
    }
  }

  "listFilesAbsolute" should {

    "return an empty list if the directory is empty" in testDir { dir =>
      dir.testRoot.listFilesAbsolute shouldBe empty
    }

    "return the list of files in the directory" in testDir { dir =>

      val f = dir.createFile()
      val d = dir.createDirectory()
      val s = dir.createSymbolicLinkTo(f)

      dir.testRoot.listFilesAbsolute should contain theSameElementsAs Seq(f, d, s)
    }

    "throw an IOException if the path is a regular file" in testDir { dir =>

      val path = dir.createFile()

      an [IOException] should be thrownBy path.listFilesAbsolute
    }

    "throw an IOException if the path is a symbolic link" in testDir { dir =>

      val d = dir.createDirectory()
      val path = dir.createSymbolicLinkTo(d)

      an [IOException] should be thrownBy path.listFilesAbsolute
    }
  }

  private class TestDir {

    private val testRootPath = Files.createTempDirectory("AbsolutePathSpec")
    private val testRootFile = testRootPath.toFile
    testRootFile.deleteOnExit()

    val testRoot = AbsolutePath(testRootPath)

    val missingPath = testRoot ++ Filename("not-there")

    val fileBytes = "This is some string".getBytes("UTF-8")

    def createSymbolicLinkTo(dest: AbsolutePath): AbsolutePath = {

      val linkPath = testRootPath.resolve("some-symlink")

      Files.createSymbolicLink(linkPath, dest.value)

      AbsolutePath(linkPath)
    }

    def createDirectory(): AbsolutePath = {

      val f = new File(testRootFile, "some-dir")

      f.mkdir() shouldBe true

      AbsolutePath(f.toPath.toAbsolutePath)
    }

    def createFile(data: Array[Byte] = fileBytes): AbsolutePath = {

      val f = new File(testRootFile, "some-file")

      FileUtils.writeByteArrayToFile(f, data)

      AbsolutePath(f.toPath.toAbsolutePath)
    }

    def cleanUp(): Unit = {
      FileUtils.deleteDirectory(testRootFile)
    }
  }

  protected def testDir(testCase: TestDir => Any) {

    val dir = new TestDir

    try {
      testCase(dir)
    } finally {
      dir.cleanUp()
    }
  }
}

