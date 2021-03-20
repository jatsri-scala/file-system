package com.scala.oop.file

abstract class DirEntry(val parentPath: String, val name: String) {
  def path: String = parentPath + Directory.SEPERATOR + name

  def asDirectory: Directory

  def getType: String
}
