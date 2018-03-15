object week7_lec {
  sealed abstract class IntList;
  case class Cons(hd: Int, tl: IntList) extends IntList;
  case class Nil() extends IntList;
  
  def range(m: Int, n: Int): IntList = {
    if (m <= n) {
      Cons(m, range(m+1,n))
    } else {
      Nil()
    }
  }                                               //> range: (m: Int, n: Int)week7_lec.IntList
  
  val D1 = range(0,1)                             //> D1  : week7_lec.IntList = Cons(0,Cons(1,Nil()))
  val D10 = range(1,10)                           //> D10  : week7_lec.IntList = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,
                                                  //| Cons(8,Cons(9,Cons(10,Nil()))))))))))
  
  def point(n: Int): IntList = range(n,n)         //> point: (n: Int)week7_lec.IntList
  
  def concat(list1: IntList, list2: IntList): IntList = list1 match {
    case Nil() => list2
    case Cons(head,tail) => Cons(head,concat(tail,list2))
  }                                               //> concat: (list1: week7_lec.IntList, list2: week7_lec.IntList)week7_lec.IntLis
                                                  //| t
  
  val S1 = concat(point(3),range(5,8))            //> S1  : week7_lec.IntList = Cons(3,Cons(5,Cons(6,Cons(7,Cons(8,Nil())))))
  
  def andL(propL: Int => Boolean, propR: Int => Boolean): Int => Boolean = {
    { x => propL(x) && propR(x) }
  }                                               //> andL: (propL: Int => Boolean, propR: Int => Boolean)Int => Boolean
  
  def orL(propL: Int => Boolean, propR: Int => Boolean): Int => Boolean = {
    { x => propL(x) || propR(x) }
  }                                               //> orL: (propL: Int => Boolean, propR: Int => Boolean)Int => Boolean
 
   def notL(prop: Int => Boolean): Int => Boolean = {
    { x => !prop(x) }
  }                                               //> notL: (prop: Int => Boolean)Int => Boolean
 
  def forallL(list: IntList, prop: Int => Boolean): Boolean = list match {
    case Nil() => true
    case Cons(head,tail) => prop(head) && forallL(tail,prop)
  }                                               //> forallL: (list: week7_lec.IntList, prop: Int => Boolean)Boolean

  def existsL(list: IntList, prop: Int => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head,tail) => prop(head) || existsL(tail,prop)
  }                                               //> existsL: (list: week7_lec.IntList, prop: Int => Boolean)Boolean
  
  val A1 = { x: Int => x > 0 }                    //> A1  : Int => Boolean = <function1>
  val A2 = { x: Int => (x*x)==(x+x) }             //> A2  : Int => Boolean = <function1>
  val C1 = existsL(range(2,2),andL(A1,A2))        //> C1  : Boolean = true
  
  sealed abstract class Prop
  case class BoolProp(b: Boolean) extends Prop
  case class PredProp(f: Int => Prop) extends Prop
  
  val P1 = PredProp({ x => PredProp({ y => BoolProp(x < y) }) })
                                                  //> P1  : week7_lec.PredProp = PredProp(<function1>)
  def propVal(prop: Prop, list: IntList): Prop = (prop,list) match {
    case (BoolProp(_),_) => prop
    case (PredProp(f),Cons(head,tail)) => propVal(f(head),tail)
    case (_,_) => prop
  }                                               //> propVal: (prop: week7_lec.Prop, list: week7_lec.IntList)week7_lec.Prop
  
  val X1 = propVal(P1,Cons(1,Cons(2,Nil())))      //> X1  : week7_lec.Prop = BoolProp(true)
  
  def and(propL: Prop, propR: Prop): Prop = (propL,propR) match {
    case (BoolProp(x),BoolProp(y)) => BoolProp(x && y)
    case (PredProp(f),BoolProp(y)) => PredProp( x => and(f(x),propR))
    case (BoolProp(x),PredProp(g)) => PredProp( x => and(propL,g(x)))
    case (PredProp(f),PredProp(g)) => PredProp( x => and(f(x),g(x)))
  }                                               //> and: (propL: week7_lec.Prop, propR: week7_lec.Prop)week7_lec.Prop
  
  val P2 = PredProp({ x => PredProp({ y => BoolProp(1 < y) }) })
                                                  //> P2  : week7_lec.PredProp = PredProp(<function1>)
  val R1 = propVal(and(P1,P2),Cons(4,Cons(3,Nil())))
                                                  //> R1  : week7_lec.Prop = BoolProp(false)
  def forall(list: IntList, prop: Prop): Prop = (list,prop) match {
    case (Nil(),_) => BoolProp(true)
    case (Cons(head,tail),PredProp(f)) => and(f(head),forall(tail,prop))
    case (_,_) => prop
  }                                               //> forall: (list: week7_lec.IntList, prop: week7_lec.Prop)week7_lec.Prop
  
  val T1 = forall(Nil(),BoolProp(false))          //> T1  : week7_lec.Prop = BoolProp(true)
  val P3 = forall(D1,P1)                          //> P3  : week7_lec.Prop = PredProp(<function1>)
  val R3 = propVal(P3,Cons(2,Nil()))              //> R3  : week7_lec.Prop = BoolProp(true)
  val R4 = propVal(P3,Cons(1,Nil()))              //> R4  : week7_lec.Prop = BoolProp(false)
  val X2 = forall(D10,P3)                         //> X2  : week7_lec.Prop = BoolProp(false)
  
  def or(propL: Prop, propR: Prop): Prop = (propL,propR) match {
    case (BoolProp(x),BoolProp(y)) => BoolProp(x || y)
    case (PredProp(f),BoolProp(y)) => PredProp( x => or(f(x),propR))
    case (BoolProp(x),PredProp(g)) => PredProp( x => or(propL,g(x)))
    case (PredProp(f),PredProp(g)) => PredProp( x => or(f(x),g(x)))
  }                                               //> or: (propL: week7_lec.Prop, propR: week7_lec.Prop)week7_lec.Prop
  
  val P4 = or(BoolProp(1 < 2),BoolProp(2 < 1))    //> P4  : week7_lec.Prop = BoolProp(true)
  
  val P5 = or(PredProp( x => BoolProp(1 < x)),PredProp( x => BoolProp(2 < 1)))
                                                  //> P5  : week7_lec.Prop = PredProp(<function1>)
  val P6 = forall(D10,P5)                         //> P6  : week7_lec.Prop = BoolProp(false)
  
  val P7 = PredProp( x => PredProp( y => BoolProp(x+y <= 11)))
                                                  //> P7  : week7_lec.PredProp = PredProp(<function1>)
  val P8 = forall(D10,forall(D1,P7))              //> P8  : week7_lec.Prop = BoolProp(true)
  val P9 = forall(D10,PredProp( x => forall(D1, PredProp( y => BoolProp(x+y <= 11)))))
                                                  //> P9  : week7_lec.Prop = BoolProp(true)
  def not(prop: Prop): Prop = prop match {
    case BoolProp(b) => BoolProp(!b)
    case PredProp(f) => PredProp( x => not(f(x)))
  }                                               //> not: (prop: week7_lec.Prop)week7_lec.Prop
  
  def exists(list: IntList, prop: Prop): Prop = (list,prop) match {
    case (Nil(),_) => BoolProp(false)
    case (Cons(head,tail),PredProp(f)) => or(f(head),exists(tail,prop))
    case (_,_) => prop
  }                                               //> exists: (list: week7_lec.IntList, prop: week7_lec.Prop)week7_lec.Prop
  
  val T2 = exists(Nil(),BoolProp(true))           //> T2  : week7_lec.Prop = BoolProp(false)


  val Predicate0 = forall(range(1,10), PredProp( x => exists(range(1,11), PredProp( y => BoolProp( x < y)))))
  val deBruinEncoding = PredProp({ y  => PredProp({ x => BoolProp( x < y)})})
  //val PredicateBruinEncoding = forall(range(1,10),exists(range1,11),deBruinEncoding)


  val Predicate1 = forall(concat(point(2), range(5,7)), PredProp( x => forall(range(1,5), PredProp(y => or(BoolProp(x>6), BoolProp(x*y <=30))))))

  val Predicate2 = PredProp( y => PredProp( x => or(BoolProp(x>6), BoolProp(x*y<=30))))
  val Range1 = concat(point(2), range(5,7))
  val Range2 = range(1,5)
  val Predicate3 = forall(Range1, forall(Range2, Predicate2))
}

