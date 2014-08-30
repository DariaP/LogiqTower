package puzzle

import scala.collection.mutable.Set
abstract class Resolver(val puzzle: Puzzle, val blocks: Set[Block]) {

  def selectBlock(position: Position): Option[Block]

  def fill(position: Position) = {
    if (blocks.isEmpty) {
      println("failure: no more blocks")
      false
    } else {
      selectBlock(position) match {
        case None => {
          println("failure: can't find block for position " + position)
          false     
        }
        case Some(block) => {
          puzzle.put(block, position)
          println("block " + block + " at position " + position)
          blocks.remove(block)
        }
      }
    }
  }
  def resolve() {
    var continue = true
    while (continue) {
      puzzle.getNextEmptyPosition match {
        case None => {
          if (puzzle.done) println("success")
          else println("failure: emply center")
          continue = false
        }
        case Some(position) => {
          if (!fill(position)) continue = false
        }
      }
    }
  }
}