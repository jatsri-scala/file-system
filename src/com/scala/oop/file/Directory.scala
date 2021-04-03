package com.scala.oop.file

import com.scala.oop.filesystem.FileSystemException

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry]) extends DirEntry(parentPath, name) {
  def hasEntry(name: String): Boolean =
    findEntry(name) != null

  // /a/b/c/d => List("a", "b", "c", "d")
  def getAllFoldersInPath: List[String] =
    path.substring(1).split(Directory.SEPERATOR).toList.filter(x => !x.isEmpty)

  def findDescendent(path: List[String]): Directory =
    if(path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendent(path.tail)

  def findDescendent(relativePath: String): Directory =
    if (relativePath.isEmpty) this
    else findDescendent(relativePath.split(Directory.SEPERATOR).toList)

  def removeEntry(entryName: String): Directory =
    if (!hasEntry(entryName)) this
    else new Directory(parentPath, name, contents.filter(x => !x.name.equals(entryName)))

  def addEntry(newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents :+ newEntry)

  def findEntry(entryName: String): DirEntry = {
    @tailrec
    def findEntryHelper(name: String, contentList: List[DirEntry]): DirEntry = {
      if (contentList.isEmpty) null
      else if(contentList.head.name.equals(name)) contentList.head
      else findEntryHelper(name, contentList.tail)
    }
    findEntryHelper(entryName, contents)
  }

  def replaceEntry(entryName: String, newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents.filter(e => !e.name.equals(entryName)) :+ newEntry)

  def isRoot: Boolean = parentPath.isEmpty

  def asDirectory: Directory = this

  def asFile: File =
    throw new FileSystemException("A directory cannot be converted into a file")

  def isDirectory: Boolean = true

  def isFile: Boolean = false

  def getType: String = "Directory"
}

object Directory {
  val SEPERATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())
}
