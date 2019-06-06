package test.inlining;


public class Demo {
  @test.Test(inputs = {1, 2, 3, 4})
  static int mainA(int n){
    int count = 0, total = 0, multiplied = 0;
    Logger logger = new PrintLogger();
    while(count < n){
      count += 1;
      multiplied = multiplied * count;
      if (multiplied < 100){
        logger.log(count);
      }
      total += factorial(5);
      total += factorial(multiplied);
      int d = factorial(count);
      if (count % 2 == 0){
        total += d;
      }

    }
    return total;
  }

  static int factorial(int x){
    if (x == 1) return 1;
    else return x * factorial(x - 1);
  }

  interface Logger{
    public Logger log(Object a);
  }
  static class PrintLogger implements Logger{
    public PrintLogger log(Object a){  System.out.println(a); return this; }
  }
  static class ErrLogger implements Logger{
    public ErrLogger log(Object a){ System.err.println(a); return this; }
  }
}


