# General Sytnax

Ski is a dynamically typed programming language built for automation that compiles down to DOS/batch script.

Statements end with semicolons.

Scoping is done using curly braces {}.

All identifiers are case sensitive.

The naming convention for identifiers is snake_case.

Files are saved as `.ski`

Ski defines a total of _____ keywords.

1. let
1. const
1. fn
1. int
1. float
1. str
1. bool
1. array
1. for
1. while
1. loop
1. break
1. continue

## Variables

Variables are declared using either the `let` or `const` keywords.
Everything is locally scoped.

`let x = 4;`

`const y = "suh";`

## Data Types

6 first class data types exist:

1. int
1. float
1. str
1. bool
1. array
1. path

## Arrays

Arrays are mutable and 0-indexed.

Arrays can be indexed using square brackets,

```rust
let x = [1, 2, 3];
print(x[0])
```

## Functions

Functions are declared using the `fn` keyword.

```rust
fn main() {
    let x = 4;
    x = 5;
}
```

```rust
fn main(a, b) {
    return a + b;
}
```

## Comments

Single line comments are created using two backslashes.

```rust
// This is a comment
fn main() {

}
```

Multi-line comments are C-style.

```rust
/*
This is a multiline comment
*/
```

## Loops

There exist 3 kinds of loops: `for`, `while`, and `loop`.

`for` loops act over collections:

```rust
// range(a, b) where `b` is exclusive, so this would loop over 0 to 9
for i in range(0, 10) {

}
```

`while` loops act while a condition is true:

```rust
while true {

}
```

`loop` loops indefinitely until given a `break` directive.

```rust
let x = 1;
loop {
    x += 1;
    if x == 2 {
        break;
    }
}
```

## Conditionals

Ski supports 2 types of conditionals: `if` and `else`. They can be combined to form an `else if` statement.

Conditionals are not wrapped in parentheses.

```rust
// this will execute
if true {
    ...
}

// this will *not* execute
if false {
    ...
}
```

Boolean operators exist as and `&&`, or `||`, and not `!`.

```rust
// This will evaluate to true
if true || false {
    ...
}

// This will evaluate to false
if true && false {
    ...
}

// This will evaluate to true
if !false {

}

// This will evaluate to false
if !true {

}
```

## Concurrency

pass

## Command Line Arguments

THIS IS IMPORTANT. COME BACK TO THIS

## Calling Other Files

pass

## File I/O

pass