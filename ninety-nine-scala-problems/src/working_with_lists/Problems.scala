package working_with_lists

/**
  * Working with List
  */
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
  def reverse[T](l: List[T]): List[T] = {
    @annotation.tailrec
    def loop[T](acum: List[T], ls: List[T]): List[T] = {
      ls match {
        case Nil => acum
        case (h :: tail) => loop(h :: acum, tail)
      }
    }    
    loop(List(), l)
  }
  
  def reverseBuiltIn[T](l: List[T]): List[T] = {
    l.reverse
  }
  //Pure functional
  def reverseFunctional[T](l: List[T]): List[T] = {
    l.foldLeft(List[T]())({case(r, h) => h :: r})
  }
  println("P05 " + reverse(List(1, 1, 2, 3, 5, 8)))  
  println("P05 " + reverse(List("Ana", "Lucia","Sofia", "Marta")))
  println("P05 " + reverse(List()))
  println("P05 " + reverseBuiltIn(List(1, 1, 2, 3, 5, 8)))
  println("P05 " + reverseBuiltIn(Nil))
  println("P05 " + reverseFunctional(List("Ana", "Lucia","Sofia", "Marta")))
  println("P05 " + reverse(List(1, 2, 3, 2, 1)))
  println("P05 " + reverse(List('r', 'e', 'c', 'o', 'n','o','c','e','r')))
  println("**********************************")  
  
  /**
   * P06 (*) Find out whether a list is a palindrome.
   * Example:
   * scala> isPalindrome(List(1, 2, 3, 2, 1))
   * scala> isPalindrome(List('r', 'e', 'c', 'o', 'n','o','c','e','r'))
   * res0: Boolean = true
   */
  def isPalindrome[T](l: List[T]): Boolean = {
    @annotation.tailrec
    def loop[T](ls: List[T], inv: List[T]): Boolean = {
      ls match {
        case Nil => true
        case (h :: tail) => if(h == inv.head) loop(tail, inv.tail)
                            else false                  
      }
    }
    if(l.isEmpty) true
    else {
      loop(l, reverse(l))
    }    
  }
  
  def isPalindromeB[T](l: List[T]): Boolean = {
    l == reverse(l)
  }  
  println("P06 " + isPalindrome(List('r', 'e', 'c', 'o', 'n','o','c','e','r')))
  println("P06 " + isPalindrome(List()))
  println("P06 " + isPalindrome(List(1)))
  println("P06 " + isPalindrome(List(1,2)))
  println("P06 " + isPalindrome(List(1,2,1)))
  println("**********************************") 
  
  /**
   * P07 (**) Flatten a nested list structure.
   * Example:
   * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)  
   */
  def flatten(l: List[Any]): List[Any] = {
    l.flatMap({case(ls: List[_]) => flatten(ls)
               case h => List(h)
      })
  }
  
  println("P07 " + flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  println("**********************************")
  
  /**
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
   * Example:
    
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */   
  def compress[T](l: List[T]): List[T] = {
    def loop[T](h: T, tail: List[T], acum: List[T]): List[T] = {
      List()
    }
    if(l.isEmpty) l
    else loop(l.head, l.tail,List())
  }
}