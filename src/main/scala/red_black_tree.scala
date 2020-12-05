import java.awt.Color
//Project : Red Black Tree
//Pariwt Huang 6180067

import scala.annotation.tailrec
class red_black_tree[T] (implicit ctx:Ordering[T]) {

  //Setting up classes for Color and Tree
  sealed abstract class Color

  case class Red() extends Color

  case class Black() extends Color

  sealed trait Tree[T] //This is the constructor of Tree

  case class Node[T](color: Color, left: Tree[T], value: T, right: Tree[T]) extends Tree[T]

  case class Leaf[T]() extends Tree[T] //This acts as "Leaf" in tree

  var root: Tree[T] = Leaf() //This is the initializer for root

  def insert[V](value: T): Unit = { //Inserting into the tree in generic function since we can't just insert value into the given tree
    this.root = insertCPS(value, root)
  }

  def blackBalanced(t : Tree[T]) : Boolean = t match {
    case Node(_,l,_,r) => blackBalanced(l) && blackBalanced(r) && blackHeight(l) == blackHeight(r)
    case Leaf() => true
  }

  //We need Ordering trait here to deal with subtypes of AnyVal
  def insertCPS(value: T, t: Tree[T]): Tree[T] = {
    t match {
      //So there are 4 cases
      //First case is for empty root node
      //The rest is comparing values recursively until it reaches the bottom leaf
      //Then we insert node there but we have to keep everything in preserved in Black_Red
      case Leaf() => new Node[T](Red(), Leaf(), value, Leaf())
      case Node(color, left, v, right) if (ctx.lt(value, v)) => preserveRedBlack(Node(color, insertCPS(value, left), v, right))
      case Node(color, left, v, right) if (ctx.gt(value, v)) => preserveRedBlack(Node(color, left, v, insertCPS(value, right)))
      case Node(color, left, v, right) if (ctx.equiv(value, v)) => preserveRedBlack(Node(color, left, v, right))
    }
  }



  def preserveRedBlack(node: Node[T]): Node[T] = node match {
    //Since in black_red trees there are rules for black and red edges therefore we have to separate these rules into
    // 5 cases
    //First case is when there is one red edge on the left connected with one red edge  on the right
    case Node(Black(), Node(Red(), left_1, val_1, Node(Red(), left_2, val_2, right_2)), value, right)
    => Node(Red(), Node(Black(), left_1, val_1, left_2), val_2, Node(Black(), right_2, value, right))

    //Second case is when there is two red edges connected together on the left side
    case Node(Black(), Node(Red(), Node(Red(), left_2, val_2, right_2), val_1, right_1), value, right)
    => Node(Red(), Node(Black(), left_2, val_2, right_2), val_1, Node(Black(), right_1, value, right))

    //Third case is when there is red edge on the right connected with one red edge on the right
    case Node(Black(), left, value, Node(Red(), left_1, val_1, Node(Red(), left_2, val_2, right_2)))
    => Node(Red(), Node(Black(), left, value, left_1), val_1, Node(Black(), left_2, val_2, right_2))

    //Fourth case is for red edge on the right connected with edge on the left
    case Node(Black(), left, value, Node(Red(), Node(Red(), left_2, val_2, right_2), val_1, right_1))
    => Node(Red(), Node(Black(), left, value, left_2), val_2, Node(Black(), right_2, val_1, right_1))

    //Fifth case
    case Node(color, left, value, right) => Node(color, left, value, right)
  }


  def heightCPS(tree: Tree[T]): Int = { //This is how we calculate the height
    def findHeight(tree: Tree[T]): Int = tree match {
      case Leaf() => 1 //
      case Node(_,l, v, r) => Seq(findHeight(l), findHeight(r)).max + 1
      case _ => 0
    }
    findHeight(root) - 1
  }
  //Size of the tree
  def size(t: Tree[T]) : BigInt = (t match {
    case Leaf() => BigInt(0)
    case Node(_, l, v, r) => size(l) + 1 + size(r)
  })ensuring(_ >= 0)

  //Function for counting height of black
  def blackHeight(t : Tree[T]) : Int = t match {
  case Leaf() => 1
  case Node(Black(), l, _, _) => blackHeight(l) + 1
  case Node(Red(), l, _, _) => blackHeight(l)
}
  //This function search for input value and given tree
  def searchTree(value: T): Boolean = {
    def helper(value: T, tree: Tree[T]): Boolean = tree match {
      case Node(_, left, v, right) if (value == v)  => true
      case Node(_, left, v, right) if (ctx.lt(value,v)) => helper(value, left)
      case Node(_, left, v, right) if (ctx.gt(value,v)) => helper(value, right)
      case Leaf() => false
      case _ => false
    }
    helper(value, root)
  }

  //This function shows how red_black tree is printed
  def printTree(): Unit = {
    //Helper function for printing process
    def helper(spacing: String, direction: String, tree: Tree[T]): String = tree match {
      case Leaf() => ("\n")
      case Node(color, left, value, right) => ( "{" + direction.toUpperCase + ": " + color + "} : " + value + "\n" + spacing + "-> " + helper("  " + spacing, "left", left) + spacing + "-> " + helper("  " + spacing, "right", right))
    }

    println(helper("", "root", root))
  }

  def Balanced(t: Tree[T]): Boolean = t match {
    // Return true or false whether given tree is balanced or not
    case Leaf() => true
    case Node(c,l,k,r) =>
      // Tree is balanced if every path has the same number of black nodes
      Balanced(l) && Balanced(r) && blackHeight(l) == blackHeight(r)
  }


  def sortedArray(): List[T] = {
    //This is a tree traversal for red_black_tree
    //We will append array accordingly
    @tailrec
    def helper(tree: Tree[T], lst: List[T]): List[T] =
      tree match {
        case Node(_, Leaf(), v, Leaf()) => helper(Leaf(), v::lst)
        case Node(_, Leaf(), v, r) => helper(r,  v :: lst)
        case Node(_, Node(_, Leaf(), value ,Leaf()), v, r) => helper(r,v:: value::lst)
        case Node(_, Node(_, left, value, right), v, r) => helper(Node(Red(), left, value, Node(Red(), right, v, r)), lst)
        case Leaf() => lst
      }

    helper(root, List[T]()).reverse
  }

  //This feature is for converting list of T's to Red Black tree
  def listToTree(lst : List[T]): Tree[T] ={
    def helper(lst: List[T],result: Tree[T]): Tree[T] ={
      if(lst.isEmpty) result
      else helper(lst.tail,insertCPS(lst.head,result))
    }
    helper(lst,Leaf())
  }



}