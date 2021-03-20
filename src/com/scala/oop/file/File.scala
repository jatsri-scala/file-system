package com.scala.oop.file

import java.nio.file.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String) extends DirEntry(parentPath, name) {
  def asDirectory: Directory =
    throw new FileSystemException("File cannot be converted to a directory")

  def asFile: File = this

  def getType: String = "File"
}

object File {
  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}
