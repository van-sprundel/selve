# Selve
The most basic programming language I could think of.

## Goal
The goal is to make use of [inkwell](https://github.com/TheDan64/inkwell) to optimise code using `LLVM`.

I'm currently in the middle of creating the language, comments aren't even supported yet!

## Usage
```
// Factorial
fn factorial(n: int) -> int {
    return match n {
        0 => 1,
        n => n * factorial(n - 1),
    };
}

// Map function
fn map(f: fn(a) -> b, list: [a]) -> [b] {
    return match list {
        [] => [],
        [x, *xs] => [f(x), *map(f, xs)]
    };
}

fn main() {
    let nums = [1, 2, 3, 4, 5];
    let squares = map(|x| x *x, numbers);

    if nums.len() == 5 {
        print(squares); // [1, 4, 9, 6, 25]
    } else {
        print("What went wrong?");
    }
}
```
