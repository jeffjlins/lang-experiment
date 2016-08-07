package fpis.ch8

/**
  * Created by jeffj on 7/19/2016.
  */
object Ch8 {

  // 8.1
  // sum == reverse sum
  // summing Nil should be 0
  // summing a list and the same list negated should be 0
  // summing a list of all same values should be value * length

  // 8.2
  // max is same if list is reversed
  // a higher negative number is not chosen
  // if all the same number, that number will be chosen
  // Nil returns 0

  // 8.3
  trait Prop { that =>
    def check: Boolean
    def &&(p: Prop) : Prop = {
      new Prop {
        override def check: Boolean = that.check && this.check
      }
    }
  }


}
