import java.awt.Color

object red_black_tree {
  //Setting up classes for Color and Tree
  sealed abstract class Color
  case class Red() extends Color
  case class Black() extends Color

  sealed abstract class Tree[T] //This is the constructor of Tree

  case class Node[T](color: Color, left: Tree[T], value : T, right : Tree[T] ) extends Tree[T]
  case class Leaf[T]() extends Tree[T] //This acts as "Leaf" in tree


}
