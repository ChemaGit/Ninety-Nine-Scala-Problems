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
  def drop[T](d: Int, l: List[T]):List[T] = {
    def dropLoop[T](n: Int, loop: List[T]): List[T] = {
      loop match {
        case Nil => Nil
        case (h :: Nil) => if (n == 1) dropLoop(d, Nil) else h :: dropLoop(n - 1, Nil)
        case (h :: tail) => if (n == 1) dropLoop(d, tail) else h :: dropLoop(n - 1, tail)
      }
    }
    dropLoop(d, l)
  }
  // Tail recursive.
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    @annotation.tailrec
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }

  // Functional.
  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  println("P016: " + drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P016: " + dropFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P016: " + dropTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  /**
    * P17 (*) Split a list into two parts.
    * The length of the first part is given. Use a Tuple for your result.
    * Example:
    * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    */
    def split[T](n: Int, l: List[T]): (List[T], List[T]) = {
      def loop[T](d: Int,lf: (List[T],List[T]) ): (List[T],List[T]) = {
        if(d == 0) (lf._1.reverse,lf._2)
        else loop(d - 1, (lf._2.head :: lf._1, lf._2.tail) )
      }
      if(n > l.length) (l,Nil)
      else if(l.isEmpty) (Nil,Nil)
      else if (l.length == n) (l, Nil)
      else {
        loop(n - 1, (List(l.head),l.tail) )
      }
    }

  // Builtin.
  def splitBuiltin[T](n: Int, ls: List[T]): (List[T], List[T]) = ls.splitAt(n)
  // Functional (barely not "builtin").
  def splitFunctional[T](n: Int, ls: List[T]): (List[T], List[T]) = (ls.take(n), ls.drop(n))

  println("P017: " + split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P017: " + splitBuiltin(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P017: " + splitFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  /**
    * P18 (**) Extract a slice from a list.
    * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list.
    * Start counting the elements with 0.
    * Example:
    * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('d, 'e, 'f, 'g)
    */
  def slice[T](lb: Int, ub: Int, l: List[T]): List[T] = {
    @annotation.tailrec
    def loop[T](min: Int,max: Int,ll: List[T],acum: List[T]): List[T] = {
      (min,max,ll) match {
        case (0,1,h::tail) => h :: acum
        case (0, _, h :: Nil) => h :: acum
        case (0, _ ,h :: tail) => loop(min, max - 1, tail, h :: acum)
        case (_, _, h :: tail) => loop(min - 1, max - 1, tail, acum)
      }
    }
    if(lb > ub) List()
    else loop(lb,ub,l,List()).reverse
  }
  // Builtin.
  def sliceBuiltin[T](lb: Int, ub: Int, ls: List[T]): List[T] =
    ls.slice(lb, ub)

  // Functional.
  def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
    ls drop s take (e - (s max 0))

  println("P018: " + slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P018: " + sliceBuiltin(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P018: " + sliceFunctional(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  /**
    * P19 (**) Rotate a list N places to the left.
    * Examples:
    * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    *
    * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    */
  def rotate[T](n: Int, l: List[T]): List[T] = {
    val bound = n % l.length
    if(bound == 0 || l.isEmpty) l
    else if(bound > 0) {
      val (head, tail) = split(bound,l)
      tail ::: head
    } else {
      val (h, t) = split(l.length + bound,l)
      t ::: h
    }
  }
  println("P019: " + rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P019: " + rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P019: " + rotate(33, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println("P019: " + rotate(-34, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  /**
    * P20 (*) Remove the Kth element from a list.
    * Return the list and the removed element in a Tuple. Elements are numbered from 0.
    * Example:
    * scala> removeAt(1, List('a, 'b, 'c, 'd))
    * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
    */
  def removeAt[T](i: Int, l: List[T]): (List[T],T) = {
    @annotation.tailrec
    def loop(acum: List[T], l: List[T],index: Int): (List[T], T) = {
      (l, index) match {
        case (h :: Nil, 0) => (acum.reverse, h)
        case (h :: tail, 0) => (acum.reverse ::: tail, h)
        case (h :: tail, _) => loop(h :: acum, tail, index - 1)
      }
    }
    if(i >= l.length) throw new NoSuchElementException
    else if(i < 0) throw new NoSuchElementException
    else loop(List(),l,i)
   }
  println("P020: " + removeAt(1, List('a, 'b, 'c, 'd)))
  println("P020: " + removeAt(5, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  /**
    * P21 (*) Insert an element at a given position into a list.
    * Example:
    * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
    * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
    */
  def insertAt[T](v: T, index: Int, l: List[T]): List[T] = {
    (index, l) match {
      case(_, Nil) => v :: l
      case (0, h :: tail) => v :: (h :: tail)
      case (_, h :: tail) => h :: insertAt(v, index - 1, tail)
    }
  }
  println("P021: " + insertAt('new, 1, List('a, 'b, 'c, 'd)))
  println("P021: " + insertAt('new, 3, List('a, 'b, 'c, 'd)))
  println("P021: " + insertAt('new, -1, List('a, 'b, 'c, 'd)))
  println("P021: " + insertAt('new, 9, List('a, 'b, 'c, 'd)))

  /**
    * P22 (*) Create a list containing all integers within a given range.
    * Example:
    * scala> range(4, 9)
    * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
    */
  def range(lowerBound: Int, upperBound: Int): List[Int] = {
    (lowerBound, upperBound) match {
      case(_, _) => {
        if(lowerBound == upperBound) List(lowerBound)
        else lowerBound :: range(lowerBound + 1, upperBound)
      }
    }
  }
  def rangeBuiltin(start: Int, end: Int): List[Int] = List.range(start, end + 1)
  def rangeTailRecursive(start: Int, end: Int): List[Int] = {
    @annotation.tailrec
    def loop(s: Int,e: Int, accum: List[Int]): List[Int] = {
      if(s == e) s :: accum
      else loop(s + 1, e, s :: accum)
    }
    loop(start, end, List()).reverse
  }
  println("P022: " + range(4, 9))
  println("P022: " + rangeTailRecursive(4, 9))

  /**
    * (**) Extract a given number of randomly selected elements from a list.
    * Example:
    *
    * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h))
    * res0: List[Symbol] = List('e, 'd, 'a)
    *
    * Hint: Use the solution to problem P20
    */
  def randomSelect[T](num: Int, ls: List[T]): List[T] = {
    def loop[T](n: Int, l: List[T],r: util.Random): List[T] = {
      if(n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(l.length), l)
        e :: loop(n - 1, rest,r)
      }
    }
    loop(num, ls,new util.Random)
  }
  println("P023: " + randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)))

  /**
    * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    * Example:
    *
    * scala> lotto(6, 49)
    * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
    */

}

