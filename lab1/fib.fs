let rec fib n = 
    match n with 
        | 1 -> 1 
        | 2 -> 1 
        | n -> fib (n - 1) + fib (n - 2)

printf "%d\n" (fib 2)
