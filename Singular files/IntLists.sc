object list {
	sealed abstract class IntList;
	case class Cons(hd: Int, tl: IntList) extends IntList
	case class Nil() extends IntList
	
	val L = Cons(1, Cons(2, Nil()))
	
	def addAll(list: IntList): Int = list match {
		case Nil() => 0
		case Cons(head, tail) => return head + addAll(tail)
	}
	
	val R1 = addAll(L)
	
	val R2 = addAll(Cons(7,L))
	
}