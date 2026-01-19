// Basic Function
let add = function(a, b) {
    return a + b;
};
console.log(add(5, 5));

// Higher Order Function
let apply = function(fn, x) {
    return fn(x);
};
let double = function(x) { return x * 2; };
console.log(apply(double, 5));

// Closure
let makeAdder = function(x) {
    return function(y) {
        return x + y;
    };
};
let addFive = makeAdder(5);
console.log(addFive(10));
console.log(addFive(20));

// Recursion (Factorial)
let factorial = function(n) {
    if (n == 0) {
        return 1;
    }
    return n * factorial(n - 1);
};
console.log(factorial(5));

// Recursion (Fibonacci)
let fib = function(n) {
    if (n == 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    }
    return fib(n - 1) + fib(n - 2);
};
console.log(fib(10));
