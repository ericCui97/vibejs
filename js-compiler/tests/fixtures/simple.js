let x = 10;
if (x > 5) {
    console.log("x is greater than 5");
    let y = x * 2;
    console.log("y is", y);
} else {
    console.log("x is small");
}

let makeAdder = function(x) {
    return function(y) {
        return x + y;
    };
};

let addFive = makeAdder(5);
console.log("addFive(10) =", addFive(10));
