// Early return in function
let earlyReturn = function() {
    return 10;
    console.log("This should not run");
    return 20;
};
console.log(earlyReturn());

// Return in If
let conditionalReturn = function(x) {
    if (x > 5) {
        return "greater";
    }
    return "less or equal";
};
console.log(conditionalReturn(10));
console.log(conditionalReturn(2));

// Nested return
let nestedReturn = function() {
    if (true) {
        if (true) {
            return "nested";
        }
    }
    return "fail";
};
console.log(nestedReturn());
