package com.gmail.jeffjlins.lexp.scala.generics.methodoverride

/**
 * Created by jlins on 1/11/16.
 */
class Base extends BaseSuper {

   def doThingInvInv(arg: Arg): Result = { new Result() }

   //TODO: need container types for covariance, contravariance and bounded (A <: B)
   //TODO: need to look at nested variance (containers of containers that use variance) - nested types might be what shows the difference between variance and bounded

   //can't do this
   //def doThingArgPlus[+A](arg: ArgContainerUnconnected[A]) = { new Result() }
   //or this
   //def doThingArgPlus[A](arg: ArgContainerUnconnected[+A]) = { new Result() }
   def doThingArgPlus[A](arg: ArgContainerUnconnected[A]) = { new Result() }


   def connectionExperiment() = {
      val c: ArgContainerConnected[Contained] = new ArgContainerConnected[Contained]
      // doesn't work
      //val d: ArgContainerConnected[Contained] = new ArgContainerConnected[ContainedImpl]
      // doesn't work
      //val e: ArgContainerConnected[Contained] = new ArgContainerConnected[ContainedSuper]

      val f: ArgContainerConnected[Contained] = new ArgContainerConnectedImpl[Contained]
      // doesn't work
      //val g: ArgContainerConnected[Contained] = new ArgContainerConnectedSuper[Contained]

      val c2: ArgContainerUnconnected[Contained] = new ArgContainerUnconnected[Contained]
      // doesn't work
      //val d2: ArgContainerUnconnected[Contained] = new ArgContainerUnconnected[ContainedImpl]
      // doesn't work
      //val e2: ArgContainerUnconnected[Contained] = new ArgContainerUnconnected[ContainedSuper]

      // KEY - doesn't work and this is the difference between Connected and Unconnected
      //val f2: ArgContainerUnconnected[Contained] = new ArgContainerUnconnectedImpl[Contained]
      // doesn't work
      //val g2: ArgContainerUnconnected[Contained] = new ArgContainerUnconnectedSuper[Contained]

   }

}
