val half=new Rational(1,2)
val quarter=new Rational(1,4)
half add quarter

val x=new Rational(1,3)
val y=new Rational(5,7)
val z=new Rational(3,2)
y add y
x sub y sub z
x less y

val a=new Rational(2)

//val strange=new Rational(1,0)
//strange add strange


class Rational(x:Int,y:Int){
  require(y!=0,"denominator must be positive")
  // assert(y!=0,"denominator must be positive")

  //secondary constructor
  def this(x:Int)=this(x,1)

  private def gcd(a:Int , b: Int):Int= if (b==0) a else gcd(b, a % b)
  private val g=gcd(x,y)
  val numer=x/g
  val denom=y/g

  def add(that:Rational)={
    new Rational(numer * that.denom + that.numer*denom,
      denom*that.denom)
  }
  
  def neg:Rational=new Rational(-numer,denom)

  def sub(that:Rational)=add(that.neg)

  def less(that:Rational)=numer*that.denom<that.numer*denom

  def max(that:Rational)=if (this less that) that else this

  override def toString={
    numer + "/" + denom
  }
}








