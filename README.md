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
 - Pre/post-dec/increment syntactic sugar
 - Print function
 - Various primitive operators
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
