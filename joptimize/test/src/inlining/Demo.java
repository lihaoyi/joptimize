package test.inlining;


public class Demo {
  @test.Test(inputs = {1, 2, 3})
  static int mainA(int n){
    return ackermann(0, n) + ackermann(n, n);
//    int count = 0, total = 0, multiplied = 0;
//    Logger logger = new PrintLogger();
//    while(count < n){
//      count += 1;
//      multiplied = multiplied * count;
//      if (multiplied < 100) logger.log(count);
//      total += ackermann(2, 2);
//      int d1 = ackermann(0, n);
//      total += d1 * multiplied;
//      int d2 = ackermann(n, count);
//      if (count % 2 == 0) total += d2;
//    }
//    return total;
  }

  static int ackermann(int m, int n){
    if (m == 0) return n + 1;
    else if (n == 0) return ackermann(m - 1, 1);
    else return ackermann(m - 1, ackermann(m, n - 1));
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


