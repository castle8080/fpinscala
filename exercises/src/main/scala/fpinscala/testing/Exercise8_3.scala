package fpinscala.testing

object Exercise8_3 {
  
  trait Prop {
  
    def check: Boolean
    
    def &&(p: Prop): Prop =
      new Prop {
        def check: Boolean = Prop.this.check && p.check
      }
    
  }
  
  object Prop {
    
    def apply(f: Boolean): Prop = new Prop {
      def check = f
    }
    
  }
  
  def main(args: Array[String]): Unit = {
    println((Prop(false) && Prop(false)).check)
    println((Prop(true) && Prop(false)).check)
    println((Prop(false) && Prop(true)).check)
    println((Prop(true) && Prop(true)).check)
  }
  
}