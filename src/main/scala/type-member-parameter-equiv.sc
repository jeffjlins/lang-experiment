import scala.collection.mutable.ArrayBuffer

// Doesn't work because not only do you not know what type
//   xs contains and what type xs.head is but it is also not
//   enforced that they are the same type so you don't know
//   if the list and what you are adding have the same type.
//def reAddHeadEx(xs: ArrayBuffer[_]): Unit =
//   xs += xs.head

// This works because you DO know what type the list contains
//   even if you don't know what type is contained in the
//   contained list. We don't need to interact directly with
//   the nested contained value so don't have a problem.
def reAddHeadContainedEx(xs: ArrayBuffer[Seq[_]]): Unit =
   xs += xs.head


// This works because even though we don't know what the type is,
//   we do know that it will be the same type every time it is used.
def reAddHeadParam[T](xs: ArrayBuffer[T]): Unit =
   xs += xs.head

// This works because even though we don't know what the type is,
// whatever type it is gets nailed down in the parameterized method
// being called.
// This is called lifting an existential to a parameterized type.
def reAddHeadExToParam(xs: ArrayBuffer[_]): Unit =
   reAddHeadParam(xs)








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


def methodT[T0](xs: MCon {type T = T0}): MCon {type T = T0} =
   xs

def methodE(xs: MCon): MCon =
   xs

val mcTyped = MCon("asdf")
val mcEx: MCon = MCon("asdf")
methodT(mcTyped)
methodE(mcTyped)
// doesn't compile because the existential is more general than
//   the typed argument required
//methodT(mcEx)
methodE(mcEx)


// The typed value can be sent to a method with an existential arg,
//   because you just make the type more general (lose type information)
// But you can't send an MCon to a method with a MCon {type T = T0}
//   arg because the argument requires a more specific type

def methodTtoE[T0](xs: MCon {type T = T0}): MCon {type T = T0} = {
   val result = methodE(xs)
   result.asInstanceOf[MCon {type T = xs.T}]
}

// does not compile because
// MCon and Mcon {type T = T0} are different types
//def methodEtoT(xs: MCon): MCon =
//   methodT(xs)

def methodEtoT(xs: MCon): MCon =
   methodT(xs.asInstanceOf[MCon {type T = xs.T}])

methodTtoE(mcTyped)
methodEtoT(mcEx)
