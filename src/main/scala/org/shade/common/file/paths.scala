package org.shade.common.file

import java.io.File
import java.nio.file.{Path, Paths}

import org.shade.common.types.{Wrapped, WrappedString}


case class Filename(value: String) extends WrappedString with Ordered[Filename] {
  require(value.nonEmpty, "Filename must not be empty")
  require(!value.contains("/"), "Filename must not contain a forward slash")

  override def compare(that: Filename) = value.compare(that.value)
}


case class RelativePath(value: Path) extends Wrapped[Path] {

  require(!value.isAbsolute, s"path is not relative: $value")

  def ++(other: RelativePath): RelativePath = RelativePath(value.resolve(other.value))

  def ++(other: Filename): RelativePath = RelativePath(value.resolve(other.value))

  lazy val file: File = value.toFile

  lazy val getName: Option[Filename] = Option(value.getFileName).map(_.toString) match {
    case Some(fn) if fn.nonEmpty => Some(Filename(fn))
    case _ => None
  }

  lazy val name: Filename = getName.getOrElse {
    throw new IllegalStateException("Cannot construct a Filename from an empty path")
  }

  lazy val isEmpty: Boolean = getName.isEmpty

  lazy val nonEmpty: Boolean = !isEmpty

  lazy val parent: RelativePath = {

    if (isEmpty) {
      throw new IllegalStateException(s"Path has no parent: $this")
    } else {
      Option(value.getParent) match {
        case Some(path) => RelativePath(path)
        case None => RelativePath.empty
      }
    }
  }
}

object RelativePath {

  val empty: RelativePath = RelativePath(Paths.get(""))

  def of(path: Path): Either[String, RelativePath] = {
    path.isAbsolute match {
      case true => Left(s"Expected a relative path, got: $path")
      case false => Right(RelativePath(path))
    }
  }
}


case class AbsolutePath(value: Path) extends Wrapped[Path] {

  require(value.isAbsolute, s"path is not absolute: $value")

  def ++(other: RelativePath): AbsolutePath = AbsolutePath(value.resolve(other.value))

  def ++(other: Filename): AbsolutePath = AbsolutePath(value.resolve(other.value))

  lazy val file: File = value.toFile

  lazy val getName: Option[Filename] = Option(value.getFileName).map(_.toString) match {
    case Some(fn) if fn.nonEmpty => Some(Filename(fn))
    case _ => None
  }

  lazy val name: Filename = getName.getOrElse {
    throw new IllegalStateException("Cannot construct a Filename from the root path")
  }

  lazy val isRoot: Boolean = getName.isEmpty

  lazy val nonRoot: Boolean = !isRoot

  lazy val parent: AbsolutePath = {

    if (isRoot) {
      throw new IllegalStateException(s"Root path has no parent: $this")
    } else {
      Option(value.getParent) match {
        case Some(path) => AbsolutePath(path)
        case None => AbsolutePath.root
      }
    }
  }

  def relativise(other: Path): RelativePath = RelativePath(value.relativize(other))

  def relativise(other: File): RelativePath = relativise(other.toPath)

  def relativise(other: AbsolutePath): RelativePath = relativise(other.value)
}

object AbsolutePath {

  val root: AbsolutePath = AbsolutePath(Paths.get("/"))

  def of(path: Path): Either[String, AbsolutePath] = {
    path.isAbsolute match {
      case true => Right(AbsolutePath(path))
      case false if path.toString.isEmpty => Left(s"Expected an absolute path, got an empty path")
      case false => Left(s"Expected an absolute path, got: $path")
    }
  }
}
