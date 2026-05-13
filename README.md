---
title: A3N Monadic Parsing
author: Lennart Andersson, Jacek Malec, Noric Couderc
---
# Assignment 3: Monadic Parsing

In this assignment you will create a parser and an interpreter for a
small imperative language. The main goal is to get acquainted with a
monadic way of solving a classical problem in computer science.

# Introduction

The following program is written in the programming language to be
parsed and interpreted in this assignment.

    read k;
    read n;
    m := 1;
    while n-m do
        begin
        if m - m/k*k then
            skip;
        else
            -- note an exponentiation below
            write m^2;
        m := m + 1; -- an inline comment
        end
    write - yet another comment, this time inside a statement!
        m^2^3;

The language has just one data type, integer, and variables are not
declared. In the `while` and `if` statements a positive expression value
is interpreted as true while 0 and negative values mean false.

The program above reads two integers, `k` and `n`, and writes squares of
all integers between `1` and `n` that are multiples of `k`.
Finally it should write the eighth power of `n`.

The grammar for the language is given by

    program ::= statements
    statement ::= variable ':=' expr ';'
            | 'skip' ';'
            | 'begin' statements 'end'
            | 'if' expr 'then' statement 'else' statement
            | 'while' expr 'do' statement
            | 'read' variable ';'
            | 'write' expr ';'
    statements ::= {statement}
    variable ::= letter {letter}

An explanation of this grammar (besides the comment case) and a parser
for an expression, `expr` (besides the power taking operation), can be
found in the document [Parsing with
Haskell](https://fileadmin.cs.lth.se/cs/Education/EDAN40/assignment3/parser.pdf), by Lennart Andersson.
The intended semantics for the language should be obvious from the keywords
for anybody familiar with any imperative language.

# Getting Started

Run `stack test`. You should see some test fail. Your task is to write code so that all the tests pass.

# Program structure

You are given the stub of a solution:

## `src/CoreParser.hs`:
defines the `Parser` type and implements the three elementary
parsers, `char`, `return` and `fail`, and the basic parser operators
`#`, `!`, `?`, `#>`, and `>->`, described in [Parsing with Haskell](https://fileadmin.cs.lth.se/cs/Education/EDAN40/assignment3/parser.pdf).

The class `Parse` with signatures for `parse`, `toString`, and
`fromString` with an implementation for the last one is introduced.

The representation of the Parser type is visible outside the module,
but this visibilty should not be exploited.

## `src/Parser.hs`:

contains a number of derived parsers and parser operators.

## `src/Expr.hs`:
contains a data type for representing an arithmetic expression, an
expression parser, an expression evaluator, and a function for
converting the representation to a string.

## `src/Dictionary.hs`:
contains a data type for representing a dictionary.

## `src/Statement.hs`:
contains a data type for representing a statement, a statement
parser, a function to interpret a list of statements, and a function
for converting the representation to a string.

## `src/Program.hs`:
contains a data type for representing a program, a program parser, a
program interpreter, and a function for converting the
representation to a string.

## `test/`
The `test` directory contains several files which check that your solution works.
The main file is `Spec.hs` which calls the other tests.

In a test using the program in the introduction with the following
definitions

    src = "read k; read n; m:=1; ... "
    p = Program.fromString src

the expression `Program.exec p [3,16]` should obviously return
`[9,36,81,144,225]`.

# Assignment and hints

1.  In `Parser.hs` implement the following functions. All the
    implementations should use other parsers and parser operators. No
    implementation may rely on the fact that the parsers return values
    of type `Maybe (a, String)`. This means e.g. that the words `Just`
    and `Nothing` may not appear in your code.

    `spaces :: Parser String.`:
        Here, we provide an example implementation.
       `spaces` accepts any number of whitespace characters as defined
        by the Prelude function `isSpace`. `spaces` treats comments as
        whitespace.

    `letter :: Parser Char.`:
    `letter` is a parser for a letter as defined by the Prelude
        function `isAlpha`.

    `chars :: Int -> Parser String.`:
       The parser `chars n` accepts `n` characters.

    `require :: String -> Parser String.`:
       The parser `require w` accepts the same string input as
        `accept w` but returns Nothing using `fail` in case of
        failure.

    `-# :: Parser a -> Parser b -> Parser b.`:
       The parser `m -# n` accepts the same input as `m # n`, but
        returns just the result from the `n` parser. The function should
        be declared as a left associative infix operator with
        precedence 7. Example:

            (accept "read" -# word) "read count;" -> Just ("count", ";")

    `#- :: Parser a -> Parser b -> Parser a.`
    :   The parser `m #- n` accepts the same input as `m # n`, but
        returns the result from the `m` parser.

2.  Implement the function `value` in module `Expr`. The expression
    `value e dictionary` should return the value of `e` if all the
    variables occur in `dictionary` and there is no division by zero.
    Otherwise an error should be reported using `error`.

3.  Implement the type and the functions in the `Statement` module. Some
    hints:
    a.  The data type `T` should have seven constructors, one for each
        kind of statement.

    b.  Define a parsing function for each kind of statement. If the
        parser has accepted the first reserved word in a statement, you
        should use `require` rather than `accept` to parse other
        reserved words or symbols in order to get better error messages
        in case of failure. An example:

               assignment = word #- accept ":=" # Expr.parse
                                 #- require ";" >-> buildAss
               buildAss (v, e) = Assignment v e

    c.  Use these functions to define `parse`.

    d.  The function
        `exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]`
        takes a list of statements to be executed, a dictionary
        containing variable/value pairs, and a list of integers
        containing numbers that may be read by `read` statements and the
        returned list contains the numbers produced by `write`
        statements.\
        The function `exec` is defined using pattern matching on the
        first argument. If it is empty an empty integer list is
        returned. The other patterns discriminate over the first
        statement in the list. As an example the execution of a
        conditional statement may be implemented by

        ```
        exec (If cond thenStmts elseStmts: stmts) dict input =
            if (Expr.value cond dict)>0
            then exec (thenStmts: stmts) dict input
            else exec (elseStmts: stmts) dict input
        ```

    e.  For each kind of statement there will be a recursive invocation
        of `exec`. A write statement will add a value to the returned
        list, while an assignment will make a recursive call with a new
        dictionary.

4.  Introduce possibility of writing comments in the code: all text
    beginning with `"--"` should be ignored until (and including) the
    newline character. You may need to modify more (or something else)
    than the `Statement` module.\
    **Hint:** It seems that the comments can be relatively simply
    handled as white space.

5.  In the `Program` module you should represent the program as a
    `Statement` list. Use the parse function from the `Statement` module
    to define the parse function in this module. Use the `exec` function
    in the `Statement` module to execute a program.

6.  Implement `toString :: T -> String` in `Statement` and `Program`.
    Please note that the output of your `toString` should be a legal
    program, i.e. should be parsable and executable again. The comments
    may be omitted, of course.

    (OPTIONAL):
    Printing a program on one line works, but it's not very readable.
    You may implement pretty printing of programs.
    A newline character should be inserted after each statement and some keywords.
    Use indentation (as in the example code above). No
    spurious empty lines should appear in the output.

7.  Extend the datatype `Expr` defined in `Expr.hs` with exponentiation,
    so that your expressions would allow e.g. a\^2 or 2\^a as legal
    strings in the program code (and compute their values when
    necessary). You will need to introduce modifications in a number of
    files.
    **Hint:** There is an informative discussion of this problem on [Stack Overflow](https://stackoverflow.com/questions/17162919/unambiguous-grammar-for-exponentiation-operation), but only the second answer (by ToxicAbe) seems to be correct!

    Make sure that your exponentiation
    1.  binds harder than multiplication or division, and
    2.  that consecutive exponentiation binds to the right, so that
        `a^b^c` is interpreted as `a^(b^c)`. E.g. 2\^3\^4 is equal
        2\^(3\^4) =2,418×10²⁴  and not (2\^3)\^4 =4 096 .

# What to submit

Submit all the files in your `src` directory to the Moodle, you can submit several files at once.

# Provided documents

Lennart Andersson's [Parsing in Haskell](https://fileadmin.cs.lth.se/cs/Education/EDAN40/assignment3/parser.pdf) describes building parsers using `Maybe`.
