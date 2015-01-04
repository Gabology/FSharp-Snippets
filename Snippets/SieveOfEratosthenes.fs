////////////////////////////
// SIEVE OF ERATOSTHENES //
//////////////////////////
module SieveOfEratosthenes


type Mark = Unmarked | Marked

let getPrimes(n) = 
    let markMultiples x (sieve: Map<_,_>) = 
        [x * x..x..n]
        |> List.fold (fun acc num -> Map.remove num acc |> Map.add num Marked) sieve

    let rec search i (sieve: Map<_,_>) = 
        if i >= int (sqrt((float n))) then sieve
        else
            match sieve.[i] with
            | Marked   -> search (i + 1) sieve // Keep searching
            | Unmarked -> search (i + 1) (markMultiples i sieve) 

    search 2 (Map.ofList [for x in 2..n -> (x, Unmarked)])
    |> Map.toList
    |> List.filter (snd >> ((=)Unmarked))
    |> List.unzip
    |> fst

let primes = getPrimes(100) 
primes |> printfn "%A"
// We will use a hashmap<int, mark> to be able to simulate a n-indexed array

(* We start with a table of numbers (e.g., 2, 3, 4, 5, . . . ) and progressively
cross off numbers in the table until the only numbers left are primes. Specifically 
we begin with the first number, p, in the table, and

1. Declare p to be prime, and cross off all the multiples of that number in the table, starting from p^2;
2. Find the next number in the table after p that is not yet crossed off and set p to that number; and then repeat from step 1.

The starting point of p^2 is a pleasing but minor optimization, which can be made 
because lower multiples will have already been crossed off when we found the primes prior to p. 
For a fixed-size table of size n, once we have reached the sqrt(n)th entry in the table, 
we need perform no more crossings off—we can simply read the remaining table entries and know them all to be prime. *)