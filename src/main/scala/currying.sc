

def cube(x:Int)=x*x*x
def fact1(n:Int)=product(x=>x)(1,n)
fact1(5)
def fact(n:Int)= {

  def f(n: Int,acc:Int): Int = {
    if (n == 0) acc else  f(n - 1,acc*n)
  }

  f(n,1)
}
def sum(f:(Int)=>Int):(Int,Int)=>Int={
  def loop(a:Int,b:Int):Int= {
    if (a > b) 0
    else f(a)+loop(a+1, b)
  }
  loop
}

def sumCurried(f:Int=>Int)(a:Int,b:Int):Int={
  if(a>b)0 else f(a)+sumCurried(f)(a+1,b)
}



//def sum(a:Int,b:Int,f:(Int)=>Int):Int={
//  def loop(a:Int,acc:Int):Int= {
//    if (a > b) acc
//    else loop(a + 1, f(a)+acc)
//  }
//  loop(a,0)
//}

def sumInts =sum(x=>x)
def sumCubes=sum(cube)
def sumDouble= sum(a=>a*a)
def sumFact=sum(fact)

def mapReduce(combine:(Int,Int)=>Int,f:Int=>Int, zero:Int)(x:Int,y:Int):Int={
  if(x>y) zero else combine(f(x),mapReduce(combine,f,zero)(x+1,y))
}

def product(f:Int=>Int)(a:Int,b:Int):Int=mapReduce((x,y)=>x*y,f,1)(a,b)
sumInts(1,4)

val y=sum(cube)


val myList=List(1,2,4,5)
def multiply=(x:Int)=>x*2

def interate(f:Int=>Int)=(l:List[Int])=>l.map(x=>f(x)).sum
val x=interate(multiply)(myList)
