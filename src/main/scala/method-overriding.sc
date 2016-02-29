class ArgSuper
class Arg extends ArgSuper
class ArgSub extends Arg
class ResultSuper
class Result extends ResultSuper
class ResultSub extends Result
class Result2

class Base {
   def doThing(arg: Arg): Result = {
      new Result()
   }
}

class BaseSub extends Base {
   override def doThing(arg: Arg): ResultSub = {
      new ResultSub()
   }
}

// why does this even work? shouldn't it be restricted to subtypes (i.e. covariance)?
class BaseSub2 extends Base {
   override def doThing(arg: Arg): Result2 = {
      new Result2()
   }
}

new BaseSub().doThing(new Arg())

new BaseSub2().doThing(new Arg())

// shouldn't this work? shouldn't arguments allow contravariance?
//class BaseSub3 extends Base {
//   override def doThing(arg: ArgSuper): Result = {
//      new Result()
//   }
//}

