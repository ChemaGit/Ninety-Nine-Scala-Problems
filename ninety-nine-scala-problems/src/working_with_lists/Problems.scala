package working_with_lists

/**
  * Working with List
  * Here we are again
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
    l match {
      case Nil => List()
      case  (h :: tail) => reverse(tail) ::: List(h)
    }
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
    def loop[T](l: List[T], lrev: List[T]): Boolean = {
      (l,lrev) match {
        case(Nil,Nil) => true
        case(h::t,hr::tr) => if(h == hr) loop(t,tr) else false
        case _ => false
      }
    }
    loop(l, l.reverse)
  }
  
  def isPalindromeB[T](l: List[T]): Boolean = {
    l == reverse(l)
  }  
  println("P06 " + isPalindrome(List('r', 'e', 'c', 'o', 'n','o','c','e','r')))
  println("P06 " + isPalindrome(List()))
  println("P06 " + isPalindrome(List(1)))
  println("P06 " + isPalindrome(List(1,2)))
  println("P06 " + isPalindrome(List(1,2,1)))
  println("P06: " + isPalindrome(List(1, 1, 2, 3, 5, 8)))
  println("P06: " + isPalindrome(List(1, 2, 3, 2, 1)))
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
    * If a list contains repeated elements they should be replaced with a single copy of the element.
    * The order of the elements should not be changed.
    * Example:
    * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
    */
  def compress[T](l: List[T]): List[T] = {
    l match {
      case (h :: Nil) => List(h)
      case (h :: tail) => if(h == tail.head) compress(tail) else h :: compress(tail)
      case (Nil) => List()
    }
  }
  println("P08: " + compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println("**********************************")


  /**
    * P09 (**) Pack consecutive duplicates of list elements into sublists.
    * If a list contains repeated elements they should be placed in separate sublists.
    * Example:
    * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    */
    def  pack[T](l: List[T]):List[List[T]] = {
      if(l == Nil) Nil
      else {
        val (packed, next) = l.span(v => v == l.head)
        if(next == Nil) List(packed)
        else packed :: pack(next)
      }
    }

    println("P09: " + pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  /**
    * P10 (*) Run-length encoding of a list.
    * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
    * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    * Example:
    * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */
  def encode[T](l: List[T]): List[(Int,T)] = {
    pack(l).map(ll => (ll.length,ll.head))
  }
  println("P010: " + encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  /**
    * P11 (*) Modified run-length encoding.
    * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
    * Only elements with duplicates are transferred as (N, E) terms.
    * Example:
    * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    */
  def encodeModified[T](l: List[T]): List[Any] = {
    pack(l).map(ll => {if(ll.length == 1) ll.head else (ll.length,ll.head)})
  }
  println("P011: " + encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  /**
    * P12 (**) Decode a run-length encoded list.
    * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    * Example:
    * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    */
  def decode[T](l: List[(Int,T)]): List[T] = {
    l match {
      case Nil => Nil
      case (h :: Nil) => if(h._1 == 1) h._2 :: decode(Nil) else h._2 :: decode(List( (h._1 - 1,h._2)))
      case (h :: tail) => if(h._1 == 1) h._2 :: decode(tail) else h._2 :: decode((h._1 - 1,h._2):: tail )
    }
  }
  def decodeBuiltIn[T](l: List[(Int,T)]): List[T] = {
    l.flatMap({case(n,v) => List.fill(n)(v)})
  }
  println("P012: " + decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  println("P012: " + decodeBuiltIn(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))

  /**
    * P13 (**) Run-length encoding of a list (direct solution).
    * Implement the so-called run-length encoding data compression method directly.
    * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    * Example:
    * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */
  def encodeDirect[T](l: List[T]): List[(Int,T)] = {
    l match {
      case Nil => Nil
      case (h :: Nil) => List((1, h))
      case (h :: tail) => {
        val (packed, next) = l.span(v => v == h)
        (packed.length,h) :: encodeDirect(next)
      }
    }
  }
  println("P013: " + encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  /**
    * P14 (*) Duplicate the elements of a list.
    * Example:
    * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
    * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    */
  def duplicate[T](l: List[T]): List[T] = {
    l.flatMap(v => List(v,v))
  }
  println("P014: " + duplicate(List('a, 'b, 'c, 'c, 'd)))

  /**
    * P15 (**) Duplicate the elements of a list a given number of times.
    * Example:
    * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    */
  def duplicateN[T](n: Int, l: List[T]): List[T] = {
    l.flatMap(v => List.fill(n)(v))
  }
  println("P015: " + duplicateN(3, List('a, 'b, 'c, 'c, 'd)))

  /**
    * P16 (**) Drop every Nth element from a list.
    * Example:
    * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    */
 }

