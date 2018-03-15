object Trees {
  sealed abstract class IntTree;
  case class Cons(root: Int, left: IntTree, right: IntTree) extends IntTree;
  case class Nil() extends IntTree;

  val T = Cons(1, Cons(2, Nil(), Nil()), Cons(3,Cons(4, Nil(), Nil()), Cons(5,Nil(),Nil())))

  def addAll(tree: IntTree): Int = tree match {
    case Nil() => 0
    case Cons(root, left, right) => root + addAll(left) + addAll(right)
  }

  val R1 = addAll(T)
  val R2 = addAll(Cons(7, T, T))


  def fold(tree: IntTree, func: (Int, Int, Int) => Int): Int = tree match {
    case Nil() => 0
    case Cons(root, left, right) => func(root, fold(left, func), fold(right, func))
  }

  def addAll1(tree: IntTree) = fold(tree, (x,y,z) => x+y+z)

  val R1_1 = addAll1(T)
  val R2_1 = addAll1(Cons(7, T, T))


  def fold2(tree: IntTree, func: (Int, Int, Int) => Int, cont: (Int) => Int): Int = tree match {
    case Nil() => cont(0)
    case Cons(root, left, right) => fold2(left, func, (l) => fold2(right, func, (r) => cont(func(root,l,r))))
  }

  def addAll2(tree: IntTree) = fold2(tree, (x,y,z) => x+y+z, (x) => x)

  val R1_2 = addAll2(T)
  val R2_2 = addAll2(Cons(7, T, T))

  def addAll3 = (tree) => addAll2(tree)

  val R1_3 = addAll3(T)
  val R2_3 = addAll3(Cons(7, T, T))




}