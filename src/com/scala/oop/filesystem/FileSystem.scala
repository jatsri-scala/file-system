package com.scala.oop.filesystem

import java.util.Scanner

import com.scala.oop.commands.Command
import com.scala.oop.file.Directory

object FileSystem extends App {
  val root = Directory.ROOT
  var state = State(root, root)
  val scanner = new Scanner(System.in)

  while(true) {
    state.show
    val input = scanner.nextLine()
    state = Command.from(input).apply(state)
  }
}
