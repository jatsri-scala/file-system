package com.scala.oop.file

import java.nio.file.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String) extends DirEntry(parentPath, name) {
  def asDirectory: Directory =
    throw new FileSystemException("File cannot be converted to a directory")

  def asFile: File = this

  def isDirectory: Boolean = false

  def isFile: Boolean = true

  def getType: String = "File"

  def setContent(newContents: String): File = {
    new File(parentPath, name, newContents)
  }

  def appendContent(newContents: String): File = {
    setContent(contents + "\n" + newContents)
  }
}

object File {
  def empty(parentPath: String, name: String): File =
    new File(parentPath, name, "")
}
