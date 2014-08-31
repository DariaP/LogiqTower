package puzzle

object PuzzleTest extends App {
  val puzzle = new Puzzle(5, 12)
  val blocks = new scala.collection.mutable.HashSet[Block]
  
  blocks.add(
    new Block(true, Array(
        List(true, false, true)),
        "red centered with 1 space"))
  blocks.add(
    new Block(true, Array(
        List(true, false, false, false, false, true)),
        "red centered with 4 spaces"))
  blocks.add(
    new Block(true, Array(
        List(true, true)),
        "blue centered with no spaces"))
  blocks.add(
    new Block(true, Array(
        List(true, false, false, false, true)),
        "blue centered with 3 spaces"))
  blocks.add(
    new Block(true, Array(
        List(true, false, false, true)),
        "green centered"))

  blocks.add(
    new Block(false, Array(
        List(true, true, true, true, true)),
        "long blue 1 level"))
  blocks.add(
    new Block(false, Array(
        List(true, true, true),
        List(false, true, true)),
        "blue 2 levels"))
  blocks.add(
    new Block(false, Array(
        List(true, true, true, false),
        List(false, false, true, true)),
        "red 2 levels 3+2"))
  blocks.add(
    new Block(false, Array(
        List(true, true, true, true),
        List(false, false, false, true)),
        "red 2 levels 4+1"))
  blocks.add(
    new Block(false, Array(
        List(true, true, true, true),
        List(false, false, true, false)),
        "green 2 levels 4+1"))
  blocks.add(
    new Block(false, Array(
        List(true, true, true),
        List(true, false, true)),
        "green 2 levels 3+2"))
  blocks.add(
    new Block(false, Array(
        List(false, true, false),
        List(true, true, true),
        List(false, false, true)),
        "blue 3 levels"))
  blocks.add(
    new Block(false, Array(
        List(false, true, false),
        List(false, true, false),
        List(true, true, true)),
        "red 3 levels"))
  blocks.add(
    new Block(false, Array(
        List(true, true, false),
        List(false, true, false),
        List(false, true, true)),
        "green 3 levels 2+1+2"))
  blocks.add(
    new Block(false, Array(
        List(true, true, false),
        List(false, true, true),
        List(false, false, true)),
        "green 3 levels 2+2+1"))

  new BruteForceResolver(puzzle, blocks, false).resolve
}