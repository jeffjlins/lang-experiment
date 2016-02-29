import scala.collection.mutable.ArrayBuffer

// a type parameter is usually more convenient and harder to screw up, but if you intend to use it existentially in most cases, changing it to a member is probably better.

final case class PCon[T](value: T) { self =>
   def myself = self
   def myExistentialSelf = self.asInstanceOf[PCon[_]]
}

// = = = = = = = = = =

sealed abstract class MCon {self =>
   type T
   val value: T
   def myself = self.asInstanceOf[MCon {type T = self.T}]
   def myExistentialSelf = self
}

object MCon {
   def apply[T0](v: T0): MCon {type T = T0} = {
      new MCon {
         type T = T0
         val value = v
      }
   }
}

PCon(3)
MCon(3)

PCon(3).value
MCon(3).value

PCon(3).value + 3
MCon(3).value + 3

println("--------------")
PCon(3).myself
MCon(3).myself
PCon(3).myself.value
MCon(3).myself.value
PCon(3).myself.value + 3
MCon(3).myself.value + 3
println("--------------")
PCon("").myExistentialSelf
MCon("").myExistentialSelf
PCon(3).myExistentialSelf.value
MCon(3).myExistentialSelf.value
// does not work because T got erased in plainMyself.value
//PCon(3).myExistentialSelf.value + 3
//MCon(3).myExistentialSelf.value + 3
