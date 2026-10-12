package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  // Exercise 1
  // -> match 3rd case
  // -> result = 1 + 2 = 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  // Exercise 8: Final result is the original list
  /*
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
  Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
  Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
  Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  Cons(1, Cons(2, Cons(3, Nil)))
  */

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 2: "remove" 1st element
  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs

  // Exercise 5: "replace" 1st element with a different value
  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => sys.error("setHead of empty list")
      case Cons(_, xs) => Cons(h, xs)

  // Exercise 3: "remove" 1st n elements
  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l // don't throw exception
    else l match
      case Nil => Nil // don't throw exception
      case Cons(_, t) => drop(t, n - 1)

  // Exercise 4: "remove" front elements as long as they match predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil => Nil // don't throw exception
      case Cons(x, t) => 
        if f(x) then dropWhile(t, f)
        else l

  // Exercise 6: returns a list that exclude last element 
  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

  // Exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  // Exercise 10
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = {
    def go(curr: B, rest: List[A]): B =
      rest match
        case Nil => curr
        case Cons(x, xs) => go(f(curr, x), xs)

    go(acc, l)
  }

  // Exercise 11: sum / product / length via foldLeft
 
  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)
    
  // Exercise 12 - approach 1
  /* 
  def reverse[A](l: List[A]): List[A] =
    l match
      case Nil => Nil
      case Cons(x, xs) => append(reverse(xs), Cons(x, Nil))
  */

  // Exercise 12 - approach 2 - with foldLeft
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A], (acc, x) => Cons(x, acc))

  // Exercise 14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (x, acc) => Cons(x, acc))

  // Exercise 15: via foldRight
  /*
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A], (x, acc) => append(x, acc))
  */

  // Exercise 15: via foldLeft
  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil:List[A], (acc, x) => append(acc, x))

  // Exercise 16:
  def incrementEach(l: List[Int]): List[Int] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, incrementEach(xs))

  // Exercise 17:
  def doubleToString(l: List[Double]): List[String] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString(), doubleToString(xs))

  // Exercise 18:
  def map[A,B](l: List[A], f: A => B): List[B] =
    l match
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs, f))

  // Exercise 19
  def filter[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil => Nil
      case Cons(x, xs) =>
        if f(x) then Cons(x, filter(xs, f))
        else filter(xs, f)
    
  // Exercise 20
  // - Example: flatMap([1, 2, 3], i => [i, i]) = [1, 1, 2, 2, 3, 3]
  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    as match
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
