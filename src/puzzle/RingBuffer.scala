package puzzle

class RingBuffer[T]() extends scala.collection.mutable.ArrayBuffer[T]{
  
  def init(initValue: T, len: Int) {
    for (i <- 0 until len) append(initValue)
  }
  def cut(from: Int, num: Int) = 
    if (from + num < length) drop(from).take(num)
    else (this ++ this).drop(from).take(num)
  def set(i: Int, value: T) {
    if (i >= length) this(i - length) = value
    else if (i < 0) this(i + length) = value
    else this(i) = value
  }
  def get(i: Int): T = 
    if (i >= length) this(i - length)
    else if (i < 0) this(i + length)
    else this(i)

}