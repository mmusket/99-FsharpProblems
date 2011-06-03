
module Arithmetic



//P31 (**) Determine whether a given integer number is prime.
//Example:
//* (is-prime 7)
//T

let isPrime number = let range = number / 2  
                     let candiates = [2..range]
                     List.map (fun x -> if number % x = 0 then false else true) candiates 
                     |> List.fold (fun acc x -> if x = false then false else acc) true

let res31a = isPrime 6
let res31b = isPrime 9
let res31c = isPrime 13
                     



//P32 (**) Determine the greatest common divisor of two positive integer numbers.
//Use Euclid's algorithm.
//Example:
//* (gcd 36 63)
//9

let gcd a b = let range = (fun (a:int) (b:int) -> if a < b then a else b) a b
              let candidates = [range .. -1 .. 1]                
              let rec biggestdivsor cand = match cand with 
                                               | [] -> 1 
                                               | h::t -> if (a % h = 0) && (b % h = 0) then  h else  biggestdivsor t
              biggestdivsor candidates

let res32 = gcd 36 63

//P33 (*) Determine whether two positive integer numbers are coprime.
//Two numbers are coprime if their greatest common divisor equals 1.
//Example:
//* (coprime 35 64)
//T

let iscoprime a b = let gfactor = gcd a b
                    if gfactor = 1 then true else false
                    
let res33 = iscoprime 35 64 


//P34 (**) Calculate Euler's totient function phi(m).
//Euler's so-called totient function phi(m) is defined as the number of 
//positive integers r (1 <= r < m) that are coprime to m.
//Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
//
//* (totient-phi 10)
//4
//
//Find out what the value of phi(m) is if m is a prime number. 
//Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). 
//In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).


let phi m = List.map (fun x -> iscoprime x m) [1..m-1] 
            |> List.fold (fun total c -> if c then (total + 1) else total) 0

let res34 = phi 10


let t m = List.map (fun x -> gcd m x) [1..m] 


//35 (**) Determine the prime factors of a given positive integer.
//Construct a flat list containing the prime factors in ascending order.
//Example:
//* (prime-factors 315)
//(3 3 5 7)


let divide a b = if a % b = 0 then [ a /b ; b] else [a] 

let decompose a = let range = [2..(a - 1)]
                  let rec step a b = match b with 
                                     | [] -> None
                                     | h::t -> if a % h = 0 then  Some([ a /h ; h]) else step a t
                  step a range


                                                     
let primeFactors a = let rec run (primes,buffer)   = match buffer with 
                                                     | [] -> primes
                                                     | h::t ->  let fact = decompose h
                                                                match fact with 
                                                                | None -> run (h :: primes , t)
                                                                | Some(f) -> run (primes , f @ t)                                             
                     run ([], [a])
                                                     

let res35 = primeFactors 20

//P36 (**) Determine the prime factors of a given positive integer (2).
//Construct a list containing the prime factors and their multiplicity.
//Example:
//* (prime-factors-mult 315)
//((3 2) (5 1) (7 1))
//Hint: The problem is similar to problem P13.
//

//from question 13 replaced previous var with an int 
let numEncode (prev, current, result) item = if item = prev then (prev, current + 1, result) else (item, 1, (current,prev)::result)
let rec numEnc l = 
                let (c,b,a)  = List.fold numEncode (-1,1, []) l
                (b, c)::a |> List.rev |> List.tail


let primeFactors2 a = a |> primeFactors |> numEnc
let res36 = primeFactors2 24


//P37 (**) Calculate Euler's totient function phi(m) (improved).
//See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of 
//problem P36 then the function phi(m) can be efficiently 
//calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) 
//can be calculated with the following formula:
//phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
//
//Note that a ** b stands for the b'th power of a.
//


let phi2 m = let factors = primeFactors2 m
             List.map (fun (m,p) -> (p - 1) * pown p (m - 1)) factors |> List.fold (fun product x -> product * x) 1

let res37 = phi2 13
let res37b = phi2 10


//P38 (*) Compare the two methods of calculating Euler's totient function.
//Use the solutions of problems P34 and P37 to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate phi(10090) as an example.


let res38 = phi 10090
let res38b = phi2 10090
// #time in interactive



//P39 (*) A list of prime numbers.
//Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
let listPrimes k = let numbers = [ 2 .. k ]
                   let findnext current nums = List.fold (fun aa k -> if k > current && aa = 0 then k else aa) 0 nums
                   let filterItems current nums = List.filter (fun x -> if (x % current = 0) && (x <> current) then false else true) nums
                   let rec doSieve current nums = let newCurrent = findnext current nums
                                                  if newCurrent = 0 then nums else 
                                                  filterItems newCurrent nums |> doSieve newCurrent
                   doSieve 1 numbers

let res39 = listPrimes 100



//P40 (**) Goldbach's conjecture.
//Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. 
//It is one of the most famous facts in number theory that has not been proved to be correct in the general case. 
//It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to
// find the two prime numbers that sum up to a given even integer.


let findPrimeSum p = let candidates = listPrimes p
                     List.fold (fun acc x -> let remainder = p - x
                                             let f = List.fold (fun aa k -> if k = remainder && aa = -1 then k else aa) -1 candidates
                                             if f <> -1 && acc = (-1,-1) then x,f else acc) (-1,-1) candidates

let res40 = findPrimeSum 18

//P41 (**) A list of Goldbach compositions.
//Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
//Example:
//* (goldbach-list 9 20)
//10 = 3 + 7
//12 = 5 + 7
//14 = 3 + 11
//16 = 3 + 13
//18 = 5 + 13
//20 = 3 + 17
//
//In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
//
//Example (for a print limit of 50):
//* (goldbach-list 1 2000 50)
//992 = 73 + 919
//1382 = 61 + 1321
//1856 = 67 + 1789
//1928 = 61 + 1867       



let listPrimeSums x y =  let range = [x .. 2 .. y] 
                         List.map (fun x ->  x, findPrimeSum x) range


let res41 = listPrimeSums 990 992;; 


let listPrimesSumsFilter x y z = listPrimeSums x y 
                                |> List.filter (fun (a,(b,c)) -> if b >= z && c >=z then true else false)

let res41b = listPrimesSumsFilter 2 2000 50
