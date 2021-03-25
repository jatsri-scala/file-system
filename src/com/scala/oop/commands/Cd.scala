package com.scala.oop.commands
import com.scala.oop.file.{DirEntry, Directory}
import com.scala.oop.filesystem.State

class Cd(dir: String) extends Command {
  override def apply(state: State): State = {
    /*
      cd /something/somethingElse/.../
      cd a/b/c - relative to the current working directoy.
    * */
    // 1. Find root
    val root = state.root
    val wd = state.wd

    println("Root directory path:" + state.root.path)
    println("Working directoy path:" + wd.path)

    // 2. Find the absolute path of the directory to CD to
    val absolutePath = {
      if(dir.startsWith(Directory.SEPERATOR)) dir
      else if(wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPERATOR + dir
    }

    // 3. Find the directory to CD to, given the path
    val destinationDirectory = doFindEntry(root, absolutePath)

    // 4. Change the state given the new Directory
    if(destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory")
    else
      State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(directory: Directory, path: String): DirEntry = {
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry = {
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    // 1. Tokens
    val tokens: List[String] = path.substring(1).split(Directory.SEPERATOR).toList

    // 2. navigate to the correct entry
    findEntryHelper(directory, tokens)
  }
}
