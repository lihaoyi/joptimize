Java
```
int foo(int n) {
    int a;
    if (n==0) {
        a = 23;
    } else {
        a = 42;
    }
    return a;
}
```

Basic Block SSA
```
int foo(int n) {
    branch(n==0, then, else)
    then:
        goto next;
    else:
        goto next;
    next:
    int a = ϕ(23 [then], 42 [else]);
    return a;
}
```

Sea of Nodes SSA
```
int foo(int n) {
    then, else = branch(n==0)
    x1 = 23
    x2 = 42
    reg = region(then, else)
    a = phi(region, x1, x2);
    return(a, reg);
}
```

Sea of Nodes SSA 2
```
int foo(int n) {
    then, else = branch(n==0)
    reg = region(then, else)
    a = phi(reg, 23, 42);
    return(a, reg);
}
```


Actual

```
region0 = region()
ctrl0, ctrl1 = if(region0, 1 < arg-1)

region1 = region(ctrl0)

region2 = region(ctrl0, ctrl1)

ctrl2 = goto(region2)

region3 = region(ctrl2)

local2 = phi(1 + 1, 2 + 1)

return(region3, local2)
```