package puzzle

class Block(val hasCenter: Boolean, val rows: Array[List[Boolean]], val name: String = "") {
  override def toString = name
  private lazy val rotatedRows = rows.reverse.map(_.reverse)
  val shouldTryToRotate = !rows.sameElements(rotatedRows)
  lazy val rotated = new Block(
      hasCenter, 
      rotatedRows,
      name + " rotated")
}