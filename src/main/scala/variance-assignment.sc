//Shoes, Tennis Shoes; Gym Bag, Red Gym Bag; Locker, Metal Locker


class ContainedSuper
class Contained extends ContainedSuper

class ContainerConnectedSuper[A]
class ContainerConnected[A] extends ContainerConnectedSuper[A]

class ContainerDisconnectedSuper[A]
class ContainerDisconnected[A] extends ContainerDisconnected

def func() = {
   val con: ContainerConnectedSuper[Contained] = new ContainerConnected[Contained]
//   val dis: ContainerDisconnectedSuper[Contained] = new ContainerDisconnected[Contained]
}



class ContainerConnectedSuper2[+A]
class ContainerConnected2[+A] extends ContainerConnectedSuper2[A]

class ContainerConnectedSuper3[+A]
class ContainerConnected3[A] extends ContainerConnectedSuper3[A]

class ContainerConnectedSuper4[A]
class ContainerConnected4[+A] extends ContainerConnectedSuper3[A]


def func2() = {
   val a2: ContainerConnectedSuper2[ContainedSuper] = new ContainerConnectedSuper2[Contained]
   val b2: ContainerConnected2[ContainedSuper] = new ContainerConnected2[Contained]
   val c2: ContainerConnectedSuper2[ContainedSuper] = new ContainerConnected2[Contained]

   val a3: ContainerConnectedSuper3[ContainedSuper] = new ContainerConnectedSuper3[Contained]
   // doesn't work
   //val b3: ContainerConnected3[ContainedSuper] = new ContainerConnected3[Contained]
   val c3: ContainerConnectedSuper3[ContainedSuper] = new ContainerConnected3[Contained]

   // doesn't work
   //val a4: ContainerConnectedSuper4[ContainedSuper] = new ContainerConnectedSuper4[Contained]
   val b4: ContainerConnected4[ContainedSuper] = new ContainerConnected4[Contained]
   // doesn't work
   //val c4: ContainerConnectedSuper4[ContainedSuper] = new ContainerConnected4[Contained]
}