package com.scala.oop.commands
import com.scala.oop.file.{DirEntry, File}
import com.scala.oop.filesystem.State

class Touch(name: String) extends CreateEntry(name) {
  override def createSpecificEntry(state: State): DirEntry =
    File.empty(state.wd.path, name)
}
