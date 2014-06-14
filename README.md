simple
======

A Simple example:

    // Generates the first 100 primes

    int n := 2;

    for int numPrimes := 0; numPrimes < 100; n++ {
        bool isPrime := true;
        for int divisor := 2; divisor < n/2 and isPrime; divisor++ {
            if divisor | n {
                isPrime := false;
            }
        }

        if isPrime {
            numPrimes++;
            printInt(n);
        }
    }

Current features:

 - Int type
 - Bool type
 - If / if-else statements
 - While loops
 - For loops
 - Pre/post-dec/increment
 - Primitive functions
    - func printInt  := int  n : void -> {/* Native code */}
    - func printBool := bool b : void -> {/* Native code */}
 - Various primitive operators
 - Default-value initialization
 - Typechecker
 - Compiles to a high-level bytecode
 - Stack-based bytecode interpreter

Partially implemented features:

 - Custom functions via lambda expressions
    [x] Parses
    [x] Typechecks
    [ ] Compiles
    [ ] Executes

Planned features:

 - Array type
 - String type
 - More compilation-time checking
    - Check initialization before assignment
    - Scope checking
    - Check no undefined variable access
    - Check lack of definition before initialization
 - Compile-time optimisations

Primitive types:

    Name | Default Value | Description
    -----+---------------+------------
    int  | 0             | Bounded integer type
    bool | false         | Boolean type
    void |               | Empty type. Can only be used as a return type
    func |               | Type of lambda expressions (i.e. functions)

Operators (in order of decreasing precedence):

    Fixity  | Symbol | Associativity | Type                 | Description
    --------+--------+---------------+----------------------+------------
    Prefix  | -      |               | int  -> int          | Negation
    Prefix  | ¬      |               | bool -> bool         | 'Not'
    Prefix  | --     |               | int  -> int          | Pre-decrement (operand must be modifiable)
    Postfix | --     |               | int  -> int          | Post-decrement (operand must be modifiable)
    Prefix  | ++     |               | int  -> int          | Pre-increment (operand must be modifiable)
    Postfix | ++     |               | int  -> int          | Post-increment (operand must be modifiable)
    Infix   | ^      | Left          | int  -> int  -> int  | Exponentiation
    Infix   | *      | Left          | int  -> int  -> int  | Multiplication
    Infix   | /      | Left          | int  -> int  -> int  | Division
    Infix   | %      | Left          | int  -> int  -> int  | Modulus
    Infix   | |      | Left          | int  -> int  -> int  | 'Divides'
    Infix   | +      | Left          | int  -> int  -> int  | Addition
    Infix   | -      | Left          | int  -> int  -> int  | Subtraction
    Infix   | =      | None          | int  -> int  -> bool | Equality
    Infix   | =/=    | None          | int  -> int  -> bool | Inequality
    Infix   | <      | None          | int  -> int  -> bool | 'Less than'
    Infix   | >      | None          | int  -> int  -> bool | 'Greater than'
    Infix   | <=     | None          | int  -> int  -> bool | 'Less than or equal to'
    Infix   | >=     | None          | int  -> int  -> bool | 'Greater than or equal to'
    Infix   | and    | Left          | bool -> bool -> bool | Conjunction (not short-circuiting)
    Infix   | or     | Left          | bool -> bool -> bool | Disjunction (not short-circuiting)
