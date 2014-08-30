package puzzle

class Block(val hasCenter: Boolean, val rows: Array[List[Boolean]], val name: String = "") {
  override def toString = name
  lazy val rotated = new Block(
      hasCenter, 
      rows.reverse.map(_.reverse),
      name + " rotated")
}