package puzzle


import scala.collection.mutable.Set

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

  def nowIsFirstMove = moves.isEmpty
  def canRollback = moves.nonEmpty
  def rollback(): Option[Move] = {
    if (moves.nonEmpty) {
      val failedMove = moves.remove(moves.length - 1)
      if (moves.nonEmpty) moves.last.failedNexts.add(failedMove.blockMove)
      else failedFirsts.add(failedMove.blockMove)
      Some(failedMove)
    } else None
  }
  
  def print() {
    println("Failed first moves:")
    failedFirsts.foreach(println(_))
    println("Moves:")
    moves.foreach(println(_))    
  }
}

class BruteForceResolver(
    override val puzzle: Puzzle, 
    override val blocks: Set[Block],
    val findAll: Boolean = true) extends Resolver(puzzle, blocks){
  var resNum = 0
  val resTree = new ResTree()

  private def selectFirstBlock = blocks.find { block => 
      resTree.canMakeMove(block, false) && block.hasCenter
  }
  private def selectNotRotatedBlock(position: Position) = 
    blocks.find { block => 
      puzzle.canPutBlock(block, position) && resTree.canMakeMove(block, false)
  }
  private def selectRotatedBlock(position: Position) = 
    blocks.find { block => 
      puzzle.canPutBlock(block.rotated, position) && 
      resTree.canMakeMove(block, true) &&
      block.shouldTryToRotate
  }  
  def selectBlock(position: Position) =
    if (resTree.nowIsFirstMove) selectFirstBlock.map(BlockMove(_, rotated = false))
    else
      selectNotRotatedBlock(position).map(BlockMove(_, rotated = false)).
      orElse(
      selectRotatedBlock(position).map(BlockMove(_, rotated = true)))

  private def rollback() = {
    resTree.rollback() match {
      case Some(Move(blockMove, position)) => {
        puzzle.remove(blockMove.inCurrentPosition,
          position
        )
        blocks.add(blockMove.block)
        true
      }
      case None => {
        resTree.print()
        false
      }
    }
  }

  def onFailure() = rollback()
  def onBlockFound(blockMove: BlockMove, position: Position) = {
    resTree.makeMove(blockMove, position)
    true
  }
  def onBlockNotFound(position: Position) = rollback()
  def onResolved() = {
    resNum += 1
    println(resNum)
    if (findAll) rollback() 
    else {
      resTree.print()
      false
    }
  }
}

object Test extends App {
  new BruteForceResolver(
      TestLogiqTower.puzzle, 
      TestLogiqTower.blocks, 
      true).resolve
}