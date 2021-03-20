package com.scala.oop.commands
import com.scala.oop.filesystem.State

class Pwd extends Command {
  override def apply(state: State): State =
    state.setMessage(state.wd.path)
}
