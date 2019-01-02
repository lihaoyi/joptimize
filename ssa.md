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
local0 = phi(ctrl0 : 0, ctrl1 : 1 + local0)
local1 = phi(ctrl0 : 1, ctrl1 : local1 * 2)

local3 = phi(ctrl0 : arg0, ctrl1 : local3)
ctrl2 = region(ctrl0, ctrl1)

ctrl3, ctrl1 = if(ctrl2, local3 >= local0)
return(ctrl3, local1)
ctrl0 = region()
```

Steps:

- Fold pass-through regions
- Fold GOTOs
- Sort by approximate topological order

```
ctrl0 = region()
ctrl1 = region(ctrl0, ctrl2)
local0 = phi(ctrl0 : 0, ctrl2 : 1 + local0)
ctrl3, ctrl2 = if(ctrl1, arg0 >= local0)

local1 = phi(ctrl0 : 1, ctrl2 : local1 * 2)
return(ctrl3, local1)
```

Goal

```
region0 = region()

ctrl0, ctrl1 = if(region0, 1 < arg-1)

region1 = region(ctrl1, ctrl0)

local0 = phi(ctrl1 : 1 + 1, ctrl0 : 2 + 1)

return(region1, local0)
```
