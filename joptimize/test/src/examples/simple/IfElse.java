package joptimize.examples.simple;


public class IfElse {
    public static int basicIf(int a, int b){
        if (a < b) return a;
        else return -a;
    }
    public static int ifNonIntZero(int a){
        if (((byte)a) > 0) return a;
        else return -a;
    }
    public static int ifNonIntBinary(int a, int b){
        if (((byte)a) > (short)b) return a;
        else return -a;
    }
    public static int ifElseIf(int a, int b){
        if (a > b) return a;
        else if (a == b) return -a;
        else return b;
    }
    public static int ifElseIfBig(int a, int b){
        if (a > b) return 1;
        else if (a > 12) return 2;
        else if (b < 10) return 3;
        else if (b == a) return 4;
        else if (b > a) return 5;
        else if (a == 10) return 6;
        else if (b == 312) return 7;
        else return 8;
    }
    public static int mathMin(int a, int b){
        return Math.min(a, b);
    }
}