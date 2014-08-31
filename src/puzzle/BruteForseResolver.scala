package puzzle


import scala.collection.mutable.Set

class BlockMove(val block: Block, val rotated: Boolean) {
  override def toString = block + { if (rotated) " rotated" else ""}
}
object BlockMove {
  def apply(block: Block, rotated: Boolean) = new BlockMove(block, rotated)
}

case class Move (val blockMove: BlockMove, val position: Position) {
  override def toString = 
    "Move block " + blockMove + 
    " on position " + position + 
    ", failed next moves: " + failedNexts.mkString(", ")
    
  val failedNexts = new scala.collection.mutable.HashSet[BlockMove]()
  def canMakeMove(block: Block, rotated: Boolean) = !failedNexts.exists {
    failedMove => failedMove.block == block && failedMove.rotated == rotated
  }
}
class ResTree {
  val moves = new scala.collection.mutable.ArrayBuffer[Move]()
  val failedFirsts = new scala.collection.mutable.HashSet[BlockMove]()
  
  def canMakeMove(block: Block, rotated: Boolean) = 
    if(moves.isEmpty) !failedFirsts.exists {
      failedMove => failedMove.block == block && failedMove.rotated == rotated
    }
    else moves.last.canMakeMove(block, rotated)

  def makeMove(blockMove: BlockMove, position: Position) {
    moves.append(new Move(blockMove, position))
  }

  def canRollback = moves.nonEmpty
  def rollback(): Option[Move] = {
    if (moves.nonEmpty) {
      val failedMove = moves.remove(moves.length - 1)
      if (moves.nonEmpty) moves.last.failedNexts.add(failedMove.blockMove)
      else failedFirsts.add(failedMove.blockMove)
      Some(failedMove)
    }
    else None
  }
  
  def print() {
    println("Failed first moves:")
    failedFirsts.foreach(println(_))
    println("Moves:")
    moves.foreach(println(_))    
  }
}

class BruteForceResolver(override val puzzle: Puzzle, override val blocks: Set[Block]) extends Resolver(puzzle, blocks){
  val resTree = new ResTree()

  def selectBlock(position: Position) = 
    blocks.find { block => 
      puzzle.canPutBlock(block, position) && resTree.canMakeMove(block, false)
  }
  def selectRotatedBlock(position: Position) = 
    blocks.find { block => 
      puzzle.canPutBlock(block.rotated, position) && 
      resTree.canMakeMove(block, true) &&
      block.shouldTryToRotate
  }

  def rollback() = {
    resTree.rollback() match {
      case Some(Move(blockMove, position)) => {
        puzzle.remove({
          if (blockMove.rotated) blockMove.block.rotated else blockMove.block
          },
          position
        )
        blocks.add(blockMove.block)
        //println("Rollback block " + block + " from position " + position)
        //resTree.print()
        true
      }
      case None => {
        //println("Cannot rollback from position")
        //resTree.print()
        false
      }
    }
  }
  override def fill(position: Position) = {
    if (blocks.isEmpty) {
      //println("failure: no more blocks")
      rollback()
    } else {
      selectBlock(position) match {
        case None => {
          selectRotatedBlock(position) match {
            case None => rollback()
            case Some(block) => {
              puzzle.put(block.rotated , position)
              //println("block " + block + " at position " + position)
              blocks.remove(block)
              resTree.makeMove(BlockMove(block, true), position)
              true              
            }
          }
        }
        case Some(block) => {
          puzzle.put(block, position)
          //println("block " + block + " at position " + position)
          blocks.remove(block)
          resTree.makeMove(BlockMove(block, false), position)
          true
        }
      }
    }
  }
  override def resolve() {
    var continue = true
    while (continue) {
      puzzle.getNextEmptyPosition match {
        case None => {
          if (puzzle.done) {
            println(":) Success: ")
            resTree.print()
            continue = false
          }
          else rollback()
        }
        case Some(position) => {
          if (!fill(position)) {
            resTree.print()
            continue = false
          }
        }
      }
    }
  }
}