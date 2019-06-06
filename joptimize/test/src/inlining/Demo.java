package test.inlining;


public class Demo {
  @test.Test(inputs = {1, 2, 3, 4})
  static int mainA(int n){
    int count = 0;
    int total = 0;
    int multiplied = 0;

    while(count < n){
      System.out.println(count);
      total += factorial(5);

      int d = factorial(n);

      if (count % 2 == 0){
        total += d;
      }

      multiplied = multiplied * count;

      total += factorial(n + multiplied) * multiplied;

      count += 1;
    }
    return total;
  }

  static int factorial(int x){
    if (x == 1) return 1;
    else return x * factorial(x - 1);

  }
}
