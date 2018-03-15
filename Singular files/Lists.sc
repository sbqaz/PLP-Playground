object PLPA_week5_lists {
  sealed abstract class IntList;
  case class Cons(hd: Int, tl: IntList) extends IntList;
  case class Nil() extends IntList;

  val L = Cons(1, Cons(2, Nil()))                 //> L  : PLPA_week5_lists.Cons = Cons(1,Cons(2,Nil()))

  def addAll(list: IntList): Int = list match {
    case Nil() => return 0
    case Cons(head,tail) => return head + addAll(tail)
  }                                               //> addAll: (list: PLPA_week5_lists.IntList)Int

  val R1 = addAll(L)                              //> R1  : Int = 3
  val R2 = addAll(Cons(7,L))                      //> R2  : Int = 10

  def foldl(list: IntList, seed: Int, func: (Int,Int) => Int): Int = list match {
    case Nil() => return seed
    case Cons(head,tail) => return foldl(tail,func(head,seed),func)
  }                                               //> foldl: (list: PLPA_week5_lists.IntList, seed: Int, func: (Int, Int) => Int)I
  //| nt

  def add(x: Int, y: Int): Int = {
    return x + y
  }                                               //> add: (x: Int, y: Int)Int

  def addAll1(list: IntList) = foldl(list,0,add)  //> addAll1: (list: PLPA_week5_lists.IntList)Int

  val R1_1 = addAll1(L)                           //> R1_1  : Int = 3
  val R2_1 = addAll1(Cons(7,L))                   //> R2_1  : Int = 10

  def addAll2(list: IntList) = foldl(list,0,{ (x,y) => x + y })
  //> addAll2: (list: PLPA_week5_lists.IntList)Int

  val R1_2 = addAll2(L)                           //> R1_2  : Int = 3
  val R2_2 = addAll2(Cons(7,L))                   //> R2_2  : Int = 10

  def foldr(list: IntList, seed: Int, func: (Int,Int) => Int): Int = list match {
    case Nil() => seed
    case Cons(head,tail) => func(head,foldr(tail,seed,func))
  }                                               //> foldr: (list: PLPA_week5_lists.IntList, seed: Int, func: (Int, Int) => Int)
  //| Int
  def addAll3(list: IntList) = foldr(list,0,add)  //> addAll3: (list: PLPA_week5_lists.IntList)Int

  val R1_3 = addAll3(L)                           //> R1_3  : Int = 3
  val R2_3 = addAll3(Cons(7,L))                   //> R2_3  : Int = 10

  def foldrc(list: IntList, seed: Int, func: (Int,Int) => Int, cont: Int => Int): Int = list match {
    case Nil() => cont(seed)
    case Cons(head,tail) => foldrc(tail,seed,func,{ (x) => cont(head + x) })
  }                                               //> foldrc: (list: PLPA_week5_lists.IntList, seed: Int, func: (Int, Int) => Int
  //| , cont: Int => Int)Int
  def addAll4(list: IntList) = foldrc(list,0,add,{ (x) => x })
  //> addAll4: (list: PLPA_week5_lists.IntList)Int
  val R1_4 = addAll4(L)                           //> R1_4  : Int = 3
  val R2_4 = addAll4(Cons(7,L))                   //> R2_4  : Int = 10

  val addAll5 = { (list) => addAll4(list) }       //> addAll5  : PLPA_week5_lists.IntList => Int = <function1>

  val R1_5 = addAll5(L)                           //> R1_5  : Int = 3
  val R2_5 = addAll5(Cons(7,L))                   //> R2_5  : Int = 10

  sealed abstract class LazyIntList;
  case class LCons(hd: Int, tail: () => LazyIntList) extends LazyIntList;
  case class LNil() extends LazyIntList;

  def lazyNil(): LazyIntList = {
    return LNil()
  }                                               //> lazyNil: ()PLPA_week5_lists.LazyIntList

  def lazyCons(hd: Int, list: LazyIntList): LazyIntList = {
    return LCons(hd, { () => list })
  }                                               //> lazyCons: (hd: Int, list: PLPA_week5_lists.LazyIntList)PLPA_week5_lists.Laz
  //| yIntList
  def lazyFoldl(list: LazyIntList, seed: Int, func: (Int,Int) => Int): Int = list match {
    case LNil() => return seed
    case LCons(head,tail) => return lazyFoldl(tail(),func(head,seed),func)
  }                                               //> lazyFoldl: (list: PLPA_week5_lists.LazyIntList, seed: Int, func: (Int, Int)
  //|  => Int)Int

  def addAll6(list: LazyIntList) = lazyFoldl(list,0,add)
  //> addAll6: (list: PLPA_week5_lists.LazyIntList)Int

  val LL = lazyCons(1, lazyCons(2, lazyNil()))    //> LL  : PLPA_week5_lists.LazyIntList = LCons(1,<function0>)
  val R1_6 = addAll6(LL)                          //> R1_6  : Int = 3
  val R2_6 = addAll6(lazyCons(7,LL))              //> R2_6  : Int = 10

  def lazyRange(from: Int, to: Int): LazyIntList = {
    if (from <= to) {
      return LCons(from,{ () => lazyRange(from+1,to) })
    } else {
      return LNil()
    }
  }                                               //> lazyRange: (from: Int, to: Int)PLPA_week5_lists.LazyIntList

  val R3 = addAll6(lazyRange(1,3))                //> R3  : Int = 6
}