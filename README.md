simple
======

A Simple example:

    // Generates the first 100 primes

    int totalPrimes := 100;
    int numPrimes := 0;
    int currentNum := 2;

    while numPrimes < totalPrimes {
        bool isPrime := true;
        int currentDivisor := 2;
        while currentDivisor < currentNum/2 and isPrime {
            if currentDivisor | currentNum {
                isPrime := false;
            }
            currentDivisor++;
        }

        if isPrime {
            numPrimes++;
            print(currentNum);
        }

        currentNum++;
    }

Current features:

 - Int type
 - Bool type
 - If / if-else statements
 - While loops
 - For loops
 - Pre/post-dec/increment
 - Print function
 - Various primitive operators
 - Default-value initialization
 - Typechecker
 - Compiles to a high-level bytecode
 - Stack-based bytecode interpreter

Planned features:
 - Array type
 - String type
 - Custom functions
 - More compilation-time checking
    - Check initialization before assignment
    - Scope checking
    - Check no undefined variable access
    - Check lack of definition before initialization
 - Compile-time optimisations

Primitive types:

    Name | Example | Description
    -----+---------+--------------------------
    int  | 42      | Bounded integer type
    bool | true    | Boolean type (true/false)

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
    Infix   | =      | Left          | int  -> int  -> bool | Equality
    Infix   | =/=    | Left          | int  -> int  -> bool | Inequality
    Infix   | <      | Left          | int  -> int  -> bool | 'Less than'
    Infix   | >      | Left          | int  -> int  -> bool | 'Greater than'
    Infix   | <=     | Left          | int  -> int  -> bool | 'Less than or equal to'
    Infix   | >=     | Left          | int  -> int  -> bool | 'Greater than or equal to'
    Infix   | and    | Left          | bool -> bool -> bool | Conjunction (not short-circuiting)
    Infix   | or     | Left          | bool -> bool -> bool | Disjunction (not short-circuiting)
