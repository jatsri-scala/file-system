package com.scala.oop.commands
import com.scala.oop.file.Directory
import com.scala.oop.filesystem.State

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    // 1. get working dir
    val wd = state.wd
    println("Working directory:", wd)
    // 2. get absolute path
    val absolutePath = {
      if (name.startsWith(Directory.SEPERATOR)) name
      else if (wd.isRoot) wd.path + name
      else wd.path + Directory.SEPERATOR + name
    }
    println("Absolute path:", absolutePath)
    // 3. Do some check
    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Nuclear war not supported yet")
    else
      doRm(state, absolutePath)
    // TODO remember to implement findDescendent(String)
  }

  def doRm(state: State, path: String): State = {
    /*
      /a => ["a"]
        path.isEmpty ? No
          path.tail.isEmpty ? Yes
            new Root without folder a

      /a/b => ["a", "b"]
        path.isEmpty ? No
          path.tail.isEmpty ? No
            nextDirectory = /a
              rmHelper(/a, ["b"])
                path.isEmpty ? No
                  path.tail.isEmpty ? Yes
                    newNextDirectory = new /a
                      newNextDirectory == nextDirectory ? No
                        root.replaceEntry("a", new /a)
    * */
    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      println("path:", path)
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDirectory = currentDirectory.findEntry(path.head)
        if (!nextDirectory.isDirectory) currentDirectory
        else {
          val newNextDirectory = rmHelper(nextDirectory.asDirectory, path.tail)
          println("newNextDirectory:", newNextDirectory.path)
          println("newNextDirectory contents:", newNextDirectory.contents.head)
          if (newNextDirectory == nextDirectory) currentDirectory
          else {
            val cd = currentDirectory.replaceEntry(path.head, newNextDirectory)
            println("cd:", cd.path)
            cd
          }
        }
      }
    }
    // 4. Find the entry to remove
    // 5. update the structure like we do for mkdir
    val tokens = path.substring(1).split(Directory.SEPERATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if(newRoot == state.root)
      state.setMessage(path + ": no such file or directory")
    else
      State(newRoot, newRoot.findDescendent(state.wd.path.substring(1)))

  }
}
