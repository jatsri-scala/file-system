package com.scala.oop.file

abstract class DirEntry(val parentPath: String, val name: String) {
  def path: String = {
    val seperatorIfNecessary =
      if(Directory.ROOT_PATH.equals(parentPath)) ""
      else Directory.SEPERATOR

    parentPath + seperatorIfNecessary  + name
  }

  def asDirectory: Directory
  def asFile: File

  def isDirectory: Boolean
  def isFile: Boolean

  def getType: String
}
