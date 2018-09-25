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
  println("P01 " + last(List(1, 1, 2, 3, 5, 8)))
  println("P01 " + lastBuiltIn(List(1, 1, 2, 3, 5, 8)))
  println("P01 " + last(List("Pepe", "Ana", "John")))
  println("P01 " + lastBuiltIn(List("Pepe", "Ana", "John")))
  println("**********************************")
  
  /**
   * P02 (*) Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[T](l: List[T]): T = {
    l match {
      case (h :: (p :: Nil)) => h
      case (h :: Nil) => throw new NoSuchElementException
      case (Nil) => throw new NoSuchElementException
      case (h :: tail) => penultimate(tail)
    }
  }
  
  def penultimateBuiltin[T](l: List[T]): T =
    if(l.isEmpty) throw new NoSuchElementException
    else l.init.last
    
  def lastNthBuiltin[T](n: Int, ls: List[T]): T = {
    if(n <= 0) throw new IllegalArgumentException
    if(ls.length < n) throw new NoSuchElementException
    ls.takeRight(n).head
  }
  
  @annotation.tailrec
  def recPenultimate[T](l: List[T]): T = {
    if(l.isEmpty) throw new NoSuchElementException
    else if(l.length == 1) throw new NoSuchElementException
    else if(l.length == 2) l.head
    else recPenultimate(l.tail)
  }
  
  println("P02 " + penultimate(List(1, 1, 2, 3, 5, 8)))
  println("P02 " + penultimate(List("Pepe", "Ana", "John")))
  println("P02 " + penultimateBuiltin(List("Ana", "Lucia")))
  println("P02 " + lastNthBuiltin(2, List("Ana", "Lucia", "Marta")))
  //println(penultimate(List()))
  println("P02 " + recPenultimate(List("Ana", "Lucia","Sofia", "Marta")))  
  println("**********************************")
  /**
   * P03 (*) Find the Kth element of a list.
   * By convention, the first element in the list is element 0.
   * Example:
   * scala> nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   */
  def nth[T](n: Int, l: List[T]): T = {
    l match {
      case(Nil) => throw new NoSuchElementException
      case _ => {
        if(n == 0) l.head
        else nth(n -1, l.tail)
      }
    }
  }
  
  def nthBuiltIn[T](n: Int, l: List[T]): T = {
    if(n >= 0) l(n) else throw new NoSuchElementException
  }
  
  println("P03 " + nth(2, List(1, 1, 2, 3, 5, 8)))
  println("P03 " + nth(5, List(1, 1, 2, 3, 5, 8)))
  println("P03 " + nth(1, List("Ana", "Lucia", "Marta")))
  println("**********************************")
  
  /**
   * P04 (*) Find the number of elements of a list.
   * Example:
   * scala> length(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 6
   */
  def length[T](l: List[T]): Int = {
    @annotation.tailrec
    def loop[T](cont: Int, l: List[T]):Int = {
      l match {
        case Nil => cont
        case _ => loop(cont + 1, l.tail)
      }
    }    
    loop(0, l)
  }
  
  def lengthBuiltIn[T](l: List[T]): Int = {
    l.length
  }
  
  def lengthFunctionalR[T](l: List[T]): Int = l.foldRight(0)({case(_, cont) => cont + 1})
  def lengthFunctionalL[T](l: List[T]): Int = l.foldLeft(0)({case(cont, _) => cont + 1})
  
  println("P04 " + length(List(1, 1, 2, 3, 5, 8)))  
  println("P04 " + length(Nil))
  println("P04 " + length(List()))
  println("P04 " + lengthFunctionalR(List(1, 1, 2, 3, 5, 8)))
  println("P04 " + lengthFunctionalL(Nil))
  println("**********************************")
  
  /**
   * P05 (*) Reverse a list.
   * Example:
   * scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
}