import scala.collection.mutable.ArrayBuffer

// why does this work for ++ but not for ++= ?
def duplicate(xs: ArrayBuffer[_]) = (xs, xs)
val dups = duplicate(ArrayBuffer("a", "b"))
// doesn't compile
//dups._1 ++= dups._2
dups._1 ++ dups._2
// doesn't compile
//duplicate(ArrayBuffer("a", "b"))._1 ++= duplicate(ArrayBuffer(1, 2))._2
duplicate(ArrayBuffer("a", "b"))._1 ++ duplicate(ArrayBuffer(1, 2))._2

//=======================================

final case class PCon[T](value: T) { self =>
   def myself = self
   def myExistentialSelf = self.asInstanceOf[PCon[_]]
}

//final case class PECon(value: _) { self =>
//   def myself = self
//}

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

// What signifigance is this? Type param becomes Any when on it's own while type member value retains reference to type member type
PCon(3).myExistentialSelf.value
//PECon(3).myself.value
MCon(3).myExistentialSelf.value
PCon(PCon(3).myExistentialSelf.value)
MCon(MCon(3).myExistentialSelf.value)
PCon(PCon(3).myExistentialSelf.value).value
MCon(MCon(3).myExistentialSelf.value).value
PCon(MCon(3).myExistentialSelf.value)
MCon(PCon(3).myExistentialSelf.value)


