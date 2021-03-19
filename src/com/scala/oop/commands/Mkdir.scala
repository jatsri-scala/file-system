package com.scala.oop.commands

import com.scala.oop.file.{DirEntry, Directory}
import com.scala.oop.filesystem.State

class Mkdir(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd

    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists!")
    } else if (name.contains(Directory.SEPERATOR)) {
      state.setMessage( name + " must not contain separators")
    } else if (checkIllegal(name)) {
      state.setMessage( name + ": illegal entry name ")
    } else {
      doMkdir(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, name: String): State = {
    /*
      /a/b
        /c
        /d
        (new) /e

      new /a
        new /b (parent /a)
          /c
          /d
          /e
    * */
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      println(path.isEmpty)
      if(path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        /*
          /a/b
            /c
            /d
            (new Entry)
          currentDirectory = /a
          path = ["b"]
        * */
        println(path)
        println("path.head")
        println(path.head)
        println("path.head.isEmpty")
        println(path.head.isEmpty)
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
        /*
          /a/b
            (contents)
            (new entry) /e

          newRoot = updateStructure(root, ["a", "b"] /e) = root.replaceEntry("a", updateStructure(/a, ["b"], /e) = /a.replaceEntry("b", updateStructure(/b, [], /e) = /b.addEntry(/e)))
            => path.isEmpty?
            => oldEntry = /a
            root.replaceEntry("a", updateStructure(/a, ["b"], /e) = /a.replaceEntry("b", updateStructure(/b, [], /e) = /b.addEntry(/e)))
              => path.isEmpty?
              => oldEntry = /b
              => /a.replaceEntry("b", updateStructure(/b, [], /e) = /b.addEntry(/e))
                => path.isEmpty ? => /b.addEntry(/e)
        * */
      }
    }

    val wd = state.wd

    // 1.All the directories in full path
    val allDirsInPath = wd.getAllFoldersInPath

    // 2.Create new directory entry in the working directory
    val newDir = Directory.empty(wd.path, name)

    // 3. Update the whole directory structure starting from the root
    // (the directory structure is IMMUTABLE)
    val newRoot = updateStructure(state.root, allDirsInPath, newDir)

    // 4. Find new working directory INSTANCE given wd's full path, in the NEW directory structure
    val newWd = newRoot.findDescendent(allDirsInPath)

    State(newRoot, newWd)
  }
}
