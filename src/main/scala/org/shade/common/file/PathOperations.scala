package org.shade.common.file

import java.io.IOException
import java.nio.file.{Files, LinkOption, Path}
import java.time.Instant
import java.util.stream.{Collectors, Stream => JavaStream}

import org.shade.common.types.NonNegativeLong

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}


trait PathOperations {

  implicit class AbsolutePathOperations(path: AbsolutePath) {

    def exists: Boolean = Files.exists(path.value, LinkOption.NOFOLLOW_LINKS)

    def isFile: Boolean = Files.isRegularFile(path.value, LinkOption.NOFOLLOW_LINKS)

    def isDirectory: Boolean = Files.isDirectory(path.value, LinkOption.NOFOLLOW_LINKS)

    def isSymbolicLink: Boolean = Files.isSymbolicLink(path.value)

    def lastModified: Instant = Files.getLastModifiedTime(path.value, LinkOption.NOFOLLOW_LINKS).toInstant

    def size: NonNegativeLong = {
      assertFile("size")
      NonNegativeLong(Files.size(path.value))
    }

    def listFilesAbsolute: Seq[AbsolutePath] = {
      assertDirectory("listFilesAbsolute")
      withResource(Files.list(path.value))(streamToSeq)
    }

    private def errorData = {
      Try(s"$path: $exists? $exists, isFile? $isFile, isDirectory? $isDirectory, isSymbolicLink? $isSymbolicLink") match {
        case Success(msg) => msg
        case Failure(e) => s"$path: Error obtaining path information (${e.getClass.getSimpleName}: ${e.getMessage}"
      }
    }

    private def streamToSeq(paths: JavaStream[Path]): Seq[AbsolutePath] = {
      paths.collect(Collectors.toList()).asScala.map(AbsolutePath.apply)
    }

    private def withResource[T <: AutoCloseable, R](resource: T)(action: T => R): R = {
      try {
        action(resource)
      } finally {
        resource.close()
      }
    }

    private def assertFile(action: String): Unit = {
      if (!isFile) throw new IOException(s"$action: Expected a file ($errorData)")
    }

    private def assertDirectory(action: String): Unit = {
      if (!isDirectory) throw new IOException(s"$action: Expected a directory ($errorData)")
    }
  }
}

object PathOperations extends PathOperations
