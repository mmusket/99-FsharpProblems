module LogicAndCodes




//P46 (**) Truth tables for logical expressions.
//Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed. Note that A and B can be Prolog goals (not only the constants true and fail).
//A logical expression in two variables can then be written in prefix notation, as in the following example: and(or(A,B),nand(A,B)).
//


let And2 a b = if a && b then true else false
let Nand2 a b = if a && b then false else true
let Or2 a b = if a || b then true else false
let Nor2 a b = if a || b then false else true

let Xor2 a b = if a <> b then true else false
// let Impl a b = if a && b then true else false
let Equ2 a b = if a = b then true else false

//Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
//
//Example:
//* table(A,B,and(A,or(A,B))).
//true true true
//true fail true
//fail true fail
//fail fail fail
//

let table f = let values = [(false,false); (false,true); (true,false); (true,true)]
              [for c in values do 
                    let a , b = c
                    yield a, b, f a b ]
              
let printResults l = for c in l do 
                        c |> (fun (a, b, c) -> printfn "%A \t %A \t=  %A" a b c )

let res46 = table And2
let res46b = table (fun a b -> And2 (Or2 a b) b) |> printResults


//P47 (*) Truth tables for logical expressions (2).
//Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
//Example:
//* table(A,B, A and (A or not B)).
//true true true
//true fail true
//fail true fail
//fail fail fail
//

let ( & )  a b = And2 a b  
let ( ^ ) a b = Or2 a b
let ( -& ) a b = Nand2 a b
let ( -^ ) a b = Nor2 a b 
let ( <-> ) a b = Xor2 a b 

//! is for unary operations 

let res47 = true -^ false & false 


//P48 (**) Truth tables for logical expressions (3).
//Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.
//Example:
//* table([A,B,C], A and (B or C) equ A and B or A and C).
//true true true true
//true true fail true
//true fail true true
//true fail fail true
//fail true true true
//fail true fail true
//fail fail true true
//fail fail fail true
//





//P49 (**) Gray code.
//An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
//n = 1: C(1) = ['0','1'].
//n = 2: C(2) = ['00','01','11','10'].
//n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
//
//Find out the construction rules and write a predicate with the following specification:
//
//% gray(N,C) :- C is the N-bit Gray code
//
//Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?

// how to do it http://cryptodox.com/Gray_code


//non cache version 

let combine a b = List.map2 (fun z y -> z + y) a b 
let front n = let size = pown 2 n 
              [ for i in [1..size] do 
                    yield if i <= size/2 then "0" else "1"]  
let symetricReverse a = a @ List.rev a

let rec buildGrayCode x = match x with 
                            | 1 -> ["0 ";"1"]
                            | k -> symetricReverse (buildGrayCode (k-1)) |> combine (front k)     
                            
 
//memoized version                         
                            
open System.Collections.Generic

let memoizedGrayCodes = let cache = new Dictionary<_, _>()                
                        let rec build n = if cache.ContainsKey(n) then cache.[n] 
                                          else let res = match n with 
                                                         | 1 -> ["0 ";"1"]
                                                         | k -> symetricReverse (build (k-1)) |> combine (front k) 
                                               cache.Add(n,res)
                                               res
                        cache, build

//builder 20;;
//Real: 00:00:03.101, CPU: 00:00:03.276, GC gen0: 56, gen1: 33, gen2: 1
//
//> builder 20;;
//Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0


//Now we build a nice wrapper for everything 

let getGrayCode n k = let cache, builder = memoizedGrayCodes
                      let codes = builder k 
                      List.toArray codes |> (fun x -> Array.get x n )

let res49 = getGrayCode 5 10

//> buildGrayCodes 4 ;;
//Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
//val it : string list =
//  ["0000 "; "0001"; "0011"; "0010 "; "0110 "; "0111"; "0101"; "0100 "; "1100 ";
//   "1101"; "1111"; "1110 "; "1010 "; "1011"; "1001"; "1000 "]
//> buildGrayCodes 4 ;;
//Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
//val it : string list =
//  ["0000 "; "0001"; "0011"; "0010 "; "0110 "; "0111"; "0101"; "0100 "; "1100 ";
//   "1101"; "1111"; "1110 "; "1010 "; "1011"; "1001"; "1000 "]




//P50 (***) Huffman code.
//First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!
//
//We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
//Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) 
//terms, where C is the Huffman code word for the symbol S. 
//In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. 
//The task shall be performed by the predicate huffman/2 defined as follows: 
//
//% huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs