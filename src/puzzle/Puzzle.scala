package puzzle

class Level (private val squaresNum: Int) {
  private var center = false
  private val squares = new RingBuffer[Boolean]
  squares.init(false, squaresNum)
  
  def centerFilled = center
  def squaresFilled = squares.forall(_ == true)
  def filled = centerFilled && squaresFilled

  def canPut(row: List[Boolean], offset: Int) = 
    squares.cut(offset, row.length).zip(row).forall(sr => 
      if (sr._2 == true ) sr._1  == false
      else true
    )
  def put(row: List[Boolean], offset: Int) {
    (0 until row.length).zip(row).foreach (
      r => if( !squares.get(r._1 + offset) ) squares.set(r._1 + offset, r._2)
    )
  }
  def remove(row: List[Boolean], offset: Int) {
    (0 until row.length).zip(row).foreach (
      r => if( squares.get(r._1 + offset) && r._2 ) squares.set(r._1 + offset, false)
    )
  }
  def fillCenter() { center = true }
  def unFillCenter() {center = false}
  def emptySquarePos = (0 until squares.length).zip(squares).find(!_._2).map(_._1)
}

class Position (val level: Int, val offset: Int) {
  override def toString = "(level " + level + ", offset " + offset + ")"
}
object Position {
  def apply(level: Int, offset: Int) = new Position(level, offset)
}

class Puzzle (private val height: Int, private val circleLen: Int) {
  private val levels = {
    val buffer = new scala.collection.mutable.ArrayBuffer[Level]()
    for(i <- 0 until height) buffer.append(new Level(circleLen))
    buffer.toArray[Level]
  }
  private def blockOffset(block: Block) = -1 * block.rows(0).takeWhile(_ == false).size
  private def matchBlock(block: Block, position: Position) = {
    if (block.hasCenter && levels(position.level).centerFilled) false
    else
      block.rows.zip(levels.drop(position.level)).forall(
          rl => rl._2.canPut(rl._1, position.offset)) 
  }
  private def findNonFilledLevel = (0 until levels.length).zip(levels).find(!_._2.filled).map(_._1)

  def canPutBlock(block: Block, position: Position) = {
    val blockPosition = Position(position.level, position.offset + blockOffset(block))
    matchBlock(block, blockPosition)
  }
  def put(block: Block, position: Position) {
    val offset = position.offset + blockOffset(block)
    if (block.hasCenter) levels(position.level).fillCenter()
    block.rows.zip(levels.drop(position.level)).foreach( rl => 
      rl._2.put(rl._1 , offset)
    )
  }
  def remove(block: Block, position: Position) {
    val offset = position.offset + blockOffset(block)
    if (block.hasCenter) levels(position.level).unFillCenter()
    block.rows.zip(levels.drop(position.level)).foreach( rl => 
      rl._2.remove(rl._1 , offset)
    )
  }
  def done = levels.forall(_.filled)
  def getNextEmptyPosition = findNonFilledLevel match {
    case None => None
    case Some(i) => levels(i).emptySquarePos.map(Position(i,_))
  }
}