package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  // Exercise 25: return number of nodes
  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  // Exercise 27: return length of longest path from root to leaf
  // (count number of edges)
  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)

  // Exercise 28:
  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  // Exercise 29: fold + size/depth/map/maximum via fold

  // - f: apply to leaf nodes
  // - g: combine results of 2 subtrees 
  def fold[B](f: A => B, g: (B,B) => B): B = this.match
    case Leaf(v) => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int =
    this.fold(_ => 1, 1 + _ + _)
  
  def depthViaFold: Int =
    this.fold(_ => 0, (ld, rd) => 1 + (ld max rd))
  
  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(v => Leaf(f(v)), (l, r) => Branch(l, r))

object Tree:

  // Exercise 25: return number of nodes
  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = ???

  // Exercise 26: return maximum value
  extension (t: Tree[Int]) def maximum: Int = {
    def go(t: Tree[Int]): Int =
      t match
        case Leaf(v) => v
        case Branch(l, r) => go(l) max go(r)
    
    go(t)
  }

  // Exercise 29: fold + size/depth/map/maximum via fold
  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(v => v, (lmax, rmax) => lmax max rmax)

