package puzzle

import scala.collection.mutable.Set
class GreedyResolver(override val puzzle: Puzzle, override val blocks: Set[Block]) 
  extends Resolver(puzzle, blocks)
{
  def selectBlock(position: Position) = blocks.find(puzzle.canPutBlock(_, position)).map(BlockMove(_, false))
  def onFailure() = false
  def onBlockFound(blockMove: BlockMove, position: Position) = true
  def onBlockNotFound(position: Position) = false
  def onResolved() = false
}