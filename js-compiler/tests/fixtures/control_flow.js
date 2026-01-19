// Boolean Logic
console.log(true);
console.log(false);
console.log(!true);
console.log(!!true);
console.log(1 < 2);
console.log(1 > 2);
console.log(1 == 1);
console.log(1 != 1);

// If/Else
if (true) {
    console.log("true is true");
} else {
    console.log("true is false (impossible)");
}

if (1 > 2) {
    console.log("1 > 2 (impossible)");
} else {
    console.log("1 is not greater than 2");
}

let x = 10;
if (x > 5) {
    console.log("x > 5");
    if (x > 15) {
        console.log("x > 15");
    } else {
        console.log("x <= 15");
    }
}
