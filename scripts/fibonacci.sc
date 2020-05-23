def fibonacci(n: Int): Long = {
  if(n == 0 || n == 1) n
  else fibonacci(n-1) + fibonacci(n-2)
}

def printAllEvenNumber(x: Int) = {
  println("Even numbers:-")
  0 to x filter(i => i%2==0) foreach(println(_))
}

0 to 10 foreach (x => println(fibonacci(x)))
printAllEvenNumber(10)