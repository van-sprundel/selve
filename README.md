# Selve

## Goal

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
