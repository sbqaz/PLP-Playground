object week7_trial {

// Introduction: Quantification
// Syntax: forall x in D. P		- P is true for each x from the set D
//         exists x in D. P		- P is true for at least one x from the set D
// Examples:
// (1) forall x in {1,...,10}. exists y in {1,...,11}. x < y
// (2) forall x in {2}\/{5,...,7}. forall y in {1,...,5}. x > 6 || x*y <= 30

// Section 0: IntLists again

  sealed abstract class IntList;
  case class Cons(hd: Int, tl: IntList) extends IntList;
  case class Nil() extends IntList;
  
  def range(m: Int, n: Int): IntList = {
    if (m <= n) {
      Cons(m, range(m+1,n))
    } else {
      Nil()
    }
  }                                               //> range: (m: Int, n: Int)week7_trial.IntList
  
  val D1 = range(0,1)                             //> D1  : week7_trial.IntList = Cons(0,Cons(1,Nil()))
  val D10 = range(1,10)                           //> D10  : week7_trial.IntList = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(
                                                  //| 7,Cons(8,Cons(9,Cons(10,Nil()))))))))))
  
  // students:
  def point(n: Int): IntList = range(n,n)         //> point: (n: Int)week7_trial.IntList
  // students:
  def concat(list1: IntList, list2: IntList): IntList = list1 match {
    case Nil() => list2
    case Cons(head,tail) => Cons(head,concat(tail,list2))
  }                                               //> concat: (list1: week7_trial.IntList, list2: week7_trial.IntList)week7_trial.
                                                  //| IntList
  
// Section 1: First-order predicate on lists

  def andL(propL: (Int) => Boolean, propR: (Int) => Boolean): (Int) => Boolean = {
    { (a: Int) => propL(a) && propR(a) }
  }                                               //> andL: (propL: Int => Boolean, propR: Int => Boolean)Int => Boolean
  
  def orL(propL: (Int) => Boolean, propR: (Int) => Boolean): (Int) => Boolean = {
    { (a: Int) => propL(a) || propR(a) }
  }                                               //> orL: (propL: Int => Boolean, propR: Int => Boolean)Int => Boolean
  
  def forallL(list: IntList, prop: (Int) => Boolean): Boolean = list match {
    case Nil() => true
    case Cons(head,tail) => prop(head) && forallL(tail,prop)
  }                                               //> forallL: (list: week7_trial.IntList, prop: Int => Boolean)Boolean
  
  // students:
  def notL(prop: (Int) => Boolean): (Int) => Boolean = {
    { (a: Int) => !prop(a) }
  }                                               //> notL: (prop: Int => Boolean)Int => Boolean

  // students:
  def existsL(list: IntList, prop: (Int) => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head,tail) => prop(head) && existsL(tail,prop)
  }                                               //> existsL: (list: week7_trial.IntList, prop: Int => Boolean)Boolean

// Section 2: First-order predicate using higher-order functions

  sealed abstract class Prop;
  case class BoolProp(b: Boolean) extends Prop
  case class PredProp(f: (Int) => Prop) extends Prop
  
  def propVal(prop: Prop, list: IntList): Prop = (prop,list) match {
    case (BoolProp(b),_) => prop
    case (PredProp(f),Cons(h,t)) => propVal(f(h),t)
    case (_,_) => prop
  }                                               //> propVal: (prop: week7_trial.Prop, list: week7_trial.IntList)week7_trial.Pro
                                                  //| p
  
  def and(propL: Prop, propR: Prop): Prop = (propL,propR) match {
    case (BoolProp(x),BoolProp(y)) => BoolProp(x && y)
    case (PredProp(f),PredProp(g)) => PredProp((a: Int) => and(f(a),g(a)))
    case (BoolProp(x),PredProp(g)) => PredProp((a: Int) => and(propL,g(a)))
    case (PredProp(f),BoolProp(y)) => PredProp((a: Int) => and(f(a),propR))
  }                                               //> and: (propL: week7_trial.Prop, propR: week7_trial.Prop)week7_trial.Prop
  
  val P1 = PredProp({ x: Int => PredProp({ y: Int => BoolProp(x < y) }) })
                                                  //> P1  : week7_trial.PredProp = PredProp(<function1>)
  val P2 = PredProp({ x: Int => PredProp({ y: Int => BoolProp(1 < y) }) })
                                                  //> P2  : week7_trial.PredProp = PredProp(<function1>)
  
  val R1 = propVal(and(P1,P2),Cons(1,Cons(2,Nil())))
                                                  //> R1  : week7_trial.Prop = BoolProp(true)
  val R2 = propVal(and(P1,P2),Cons(2,Cons(1,Nil())))
                                                  //> R2  : week7_trial.Prop = BoolProp(false)
  
  def forall(list: IntList, prop: Prop): Prop = (list,prop) match {
    	case (Nil(),_) => BoolProp(true)
    	case (Cons(head,tail),PredProp(f)) => and(f(head),forall(tail,prop))
		case (_,_) => prop
  }                                               //> forall: (list: week7_trial.IntList, prop: week7_trial.Prop)week7_trial.Prop
                                                  //| 
  val T1 = forall(Nil(),BoolProp(false))          //> T1  : week7_trial.Prop = BoolProp(true)
  
  val P3 = forall(D1,P1)                          //> P3  : week7_trial.Prop = PredProp(<function1>)
  val R3 = propVal(P3,Cons(2,Nil()))              //> R3  : week7_trial.Prop = BoolProp(true)
  val R4 = propVal(P3,Cons(1,Nil()))              //> R4  : week7_trial.Prop = BoolProp(false)
 
  def or(propL: Prop, propR: Prop): Prop = (propL,propR) match {
    case (BoolProp(x),BoolProp(y)) => BoolProp(x || y)
    case (PredProp(f),PredProp(g)) => PredProp((a: Int) => or(f(a),g(a)))
    case (BoolProp(x),PredProp(g)) => PredProp((a: Int) => or(propL,g(a)))
    case (PredProp(f),BoolProp(y)) => PredProp((a: Int) => or(f(a),propR))
  }                                               //> or: (propL: week7_trial.Prop, propR: week7_trial.Prop)week7_trial.Prop
  
  val P4 = or(BoolProp(1 < 2),BoolProp(2 < 1))    //> P4  : week7_trial.Prop = BoolProp(true)
  val P5 = or(PredProp{ x: Int => BoolProp(1 < 2)},BoolProp(2 < 1))
                                                  //> P5  : week7_trial.Prop = PredProp(<function1>)
  val P6 = forall(D1,P5)                          //> P6  : week7_trial.Prop = BoolProp(true)
  
  val P7 = PredProp({ x: Int => PredProp({ y: Int => BoolProp(x+y <= 11) }) })
                                                  //> P7  : week7_trial.PredProp = PredProp(<function1>)
  val P8 = forall(D10,forall(D1, P7))             //> P8  : week7_trial.Prop = BoolProp(true)
  val P9 = forall(D10,PredProp({ x: Int => forall(D1, PredProp({ y: Int => BoolProp(x+y <= 11) })) }))
                                                  //> P9  : week7_trial.Prop = BoolProp(true)

  // students:
  def not(prop: Prop): Prop = prop match {
    case BoolProp(x) => BoolProp(!x)
    case PredProp(f) => PredProp((a: Int) => not(f(a)))
  }                                               //> not: (prop: week7_trial.Prop)week7_trial.Prop
  // students
  def exists(list: IntList, prop: Prop): Prop = (list,prop) match {
    	case (Nil(),_) => BoolProp(false)
    	case (Cons(head,tail),PredProp(f)) => or(f(head),exists(tail,prop))
		case (_,_) => prop
  }                                               //> exists: (list: week7_trial.IntList, prop: week7_trial.Prop)week7_trial.Prop
                                                  //| 
  val T2 = exists(Nil(),BoolProp(true))           //> T2  : week7_trial.Prop = BoolProp(false)
  
  // Exercises:
  // (1) forall x in {1,...,10}. exists y in {1,...,11}. x < y
  
  val Q1 = forall(D10,PredProp({ x: Int => exists(range(1,11),PredProp({ y: Int => BoolProp(x < y) })) }))
                                                  //> Q1  : week7_trial.Prop = BoolProp(true)
  val Q2 = PredProp({ y: Int => PredProp({ x: Int => BoolProp(x < y) }) })
                                                  //> Q2  : week7_trial.PredProp = PredProp(<function1>)
  val Q3 = forall(D10,exists(range(1,11),Q1))     //> Q3  : week7_trial.Prop = BoolProp(true)
  
  // (2) forall x in {2}\/{5,...,7}. forall y in {1,...,5}. x > 6 || x*y <= 30
  
  val Q4 = PredProp({ y: Int => PredProp({ x: Int => or(BoolProp(x>6),BoolProp(x*y <= 30)) }) })
                                                  //> Q4  : week7_trial.PredProp = PredProp(<function1>)
  val D2 = concat(point(2),range(5,7))            //> D2  : week7_trial.IntList = Cons(2,Cons(5,Cons(6,Cons(7,Nil()))))
  val D3 = range(1,5)                             //> D3  : week7_trial.IntList = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil())))))
  val Q5 = forall(D2,forall(D3,Q4))               //> Q5  : week7_trial.Prop = BoolProp(true)
}