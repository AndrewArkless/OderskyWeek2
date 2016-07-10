def sumInts(a:Int, b:Int):Int={
  if(a>b)0 else a + sumInts(a+1,b)
}

sumInts(1,20)

def cube(x:Int)=x*x*x

def sumCubes(a:Int,b:Int):Int={
  if (a>b) 0 else cube(a)+sumCubes(a+1,b)
}

def fact(n:Int)= {
  def f(n: Int,acc:Int): Int = {
    if (n == 0) acc else  f(n - 1,acc*n)
  }

  f(n,1)
}

def sum(a:Int,b:Int,f:(Int)=>Int):Int={
  def loop(a:Int,acc:Int):Int= {
    if (a > b) acc
    else loop(a + 1, f(a)+acc)
  }
  loop(a,0)
}

sum(1,4,x=>x)
sum(1,20,fact)

def sumDouble(x:Int,y:Int)={
  sum(x,y,a=>a*a)
}

def sumFact(x:Int,y:Int)={
  sum(x,y,a=>fact(a))
}