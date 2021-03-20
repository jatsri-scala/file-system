package com.scala.oop.commands

import com.scala.oop.file.{DirEntry, Directory}
import com.scala.oop.filesystem.State

class Mkdir(name: String) extends CreateEntry(name) {
  override def createSpecificEntry(state: State): DirEntry =
    Directory.empty(state.wd.path, name)
}
