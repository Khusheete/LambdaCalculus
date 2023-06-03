# What is this repo ?

This is an interpreter/repl for a custom $\lambda$-calculus dialect I made to play around with this very interesting bit of mathematics.


# Build

To build the program use `make build` (you may need to use this command twice for it to work - I didn't spend a lot of time on the build system, and stopped when it somewhat worked). The output program will be created at `./bin/lambda-calc`.
You may also need to `make clean` before building the program if you changed it.

# Usage

Launching the program in a terminal will put you in a REPL. You can give it files as arguments, and they will be executed before launching the REPL.


# Syntax

- Variables are a string of utf8 characters (except parenthesis, whitespaces and newlines)
- Abstractions are denoted with the arrow syntax: `arg -> expr`
- Applications are the same as in $\lambda$-calculus: two expressions separated by a space
- Define a variable with `var = expr`. <u>/!\\</u> Spaces are important, `var= expr` will be interpreted as: "apply `expr` to `var=`".
- Comments start with `#` (same as python or julia)


## Examples

| $\lambda$-calculus | This program             |
| ------------------ | ------------------------ |
| $\lambda x. M$     | $x$ -> $M$               |
| $a$ $b$            | $a$ $b$                  |
| $\lambda ab. a$    | $a$ -> $b$ -> a          |
| $\lambda f. ff$    | $f$ -> $f$ $f$           |

The file `lambda.lb` contains more lambda expressions (most are usual lambda exressions, such as church numerals or boolean representation, some are custom).


# License

This software is under the MIT License.