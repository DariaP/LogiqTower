package puzzle

import scala.collection.mutable.Set

case class BlockMove(val block: Block, val rotated: Boolean) {
  def inCurrentPosition = { if (rotated) block.rotated else block }
  override def toString = block + { if (rotated) " rotated" else ""}
}

trait Logger {
  def log(message: String) {}
}
abstract class Resolver(val puzzle: Puzzle, val blocks: Set[Block]) extends Logger{

  def selectBlock(position: Position): Option[BlockMove]
  def onBlockFound(blockMove: BlockMove, position: Position): Boolean
  def onBlockNotFound(position: Position): Boolean
  def onResolved(): Boolean
  def onFailure(): Boolean
  
  def fill(position: Position) = {
    if (blocks.isEmpty) {
      log("failure: no more blocks")
      onBlockNotFound(position)
    } else {
      selectBlock(position) match {
        case None => {
          log("failure: can't find block for position " + position)
          onBlockNotFound(position)  
        }
        case Some(blockMove) => {
          log("block found " + blockMove.inCurrentPosition + " for position " + position)
          puzzle.put(
            blockMove.inCurrentPosition , 
            position)
          blocks.remove(blockMove.block)
          onBlockFound(blockMove, position)
        }
      }
    }
  }

  def resolve() {
    var continue = true
    while (continue) {
      puzzle.getNextEmptyPosition match {
        case None => {
          if (puzzle.done) {
            log("success")
            continue = onResolved()
          } else {
            log("failure: emply center")
            continue = onFailure()
          }
        }
        case Some(position) => {
          continue = fill(position)
        }
      }
    }
  }
}