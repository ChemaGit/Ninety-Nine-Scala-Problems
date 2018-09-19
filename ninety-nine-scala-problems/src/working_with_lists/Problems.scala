package working_with_lists

object Problems extends App{
  /**
   * P01 (*) Find the last element of a list.
   * Example:
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   * scala> last(List("Pepe", "Ana", "John"))
   * res0: String = "John"
   * [T] ==> method can accept type String, Int, Double ....
   */
  def last[T](l: List[T]): T = {
    l match {
      case (h :: Nil) => h 
      case (h :: tail) => last(tail)      
      case _ => throw new NoSuchElementException
    }    
  }
  
  def lastBuiltIn[T](l: List[T]): T = {
    l.last
  }
  println(last(List(1, 1, 2, 3, 5, 8)))
  println(lastBuiltIn(List(1, 1, 2, 3, 5, 8)))
  println(last(List("Pepe", "Ana", "John")))
  println(lastBuiltIn(List("Pepe", "Ana", "John")))
  
  /**
   * P02 (*) Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
}