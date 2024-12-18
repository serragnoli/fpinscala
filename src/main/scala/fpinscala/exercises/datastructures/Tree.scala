package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match {
    case Leaf(value) => 0
    case Branch(left, right) => {
      val lDepth = 1 + left.depth
      val rDepth = 1 + right.depth
      if (lDepth > rDepth) lDepth else rDepth
    }
  }

  def map[B](f: A => B): Tree[B] = ???

  def fold[B](f: A => B, g: (B, B) => B): B = ???

  def sizeViaFold: Int = ???

  def depthViaFold: Int = ???

  def mapViaFold[B](f: A => B): Tree[B] = ???

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = ???

  extension (t: Tree[Int])
    def maximum: Int = t match {
      case Leaf(v) => v
      case Branch(left, right) => {
        val lMax = left.maximum
        val rMax = right.maximum
        if (lMax > rMax) lMax else rMax
      }
    }

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
