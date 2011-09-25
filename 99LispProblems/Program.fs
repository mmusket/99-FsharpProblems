// Learn more about F# at http://fsharp.net
module questions

let theList = [1..5]
let items = ['a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'a'; 'a'; 'c'; 'd'; 'd'; 'e'; 'e';] 


//Last item in a list 
let lastItem aList = aList |> List.rev |> List.head                      
let res1 = lastItem theList


//Find the last but one box of a list.
//Example:
//* (my-but-last '(a b c d))
//(C D)

let firstTwo aList = match aList with 
                        |[] -> []
                        |h::t ->  let secondHead = List.head t
                                  [h ; secondHead]                                      
                                  
let res2 = theList |> List.rev |> firstTwo |> List.rev                                 


//Find the K'th element of a list.
//The first element in the list is number 1.
//Example:
//* (element-at '(a b c d e) 3)
//C


let rec getPos l c =  if c = 0 then List.head l else getPos (List.tail l) (c - 1)
let res3 = getPos [1..10] 4


// Find the number of elements of a list.

let ListSize l = List.fold (fun b _ -> (b + 1)) 0 l

let res4 = ListSize [1..5]


//Reverse a list 

let rev l = List.rev l
let res5 = rev [1..5]

//  Find out whether a list is a palindrome.
//A palindrome can be read forward or backward; e.g. (x a m a x).

let compareItems l m = List.fold2 (fun a l m -> if l = m  then a else false) true l m
                   
let isPalindrome a =  a |> List.rev |> compareItems a 

let res6T = isPalindrome [1;2;3;2;1] 
let res6F = isPalindrome [1..5] 


//Flatten a nested list structure.
//Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

//Example:
//* (my-flatten '(a (b (c d) e)))
//(A B C D E)



let nestedList=  [[[1;2];[3;4]];[[5;6];[7;8]]]

                
type NestedListElement<'T> = //'
    | L of NestedListElement<'T> list //'
    | V of 'T //'
let nested = [L[L[V 1;V 2]; V 3]; V 4; L[L[V 5;V 6]; V 7]] 
let flatten n1 = 
                let rec traverseNestedList nl = match nl with      
                                                | V c -> [c]           
                                                | L a -> List.collect traverseNestedList a
                List.collect traverseNestedList n1 
let res7 = nested |> flatten


// Eliminate consecutive duplicates of list elements.
//If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
//
//Example:
//* (compress '(a a a a b c c a a d e e e e))
//(A B C A D E)

                           
let compress input = let removeRepetions (out, rep) l = List.fold (fun (out, rep) l -> if l = rep then (out, l) else (l :: out, l)) (out,rep) l
                     let revcomp = items |> removeRepetions ([], ' ')
                     match revcomp with 
                     | (a,_) -> List.rev a

let res8 = items |> compress


//P09 (**) Pack consecutive duplicates of list elements into sublists.
//If a list contains repeated elements they should be placed in separate sublists.
//
//Example:
//* (pack '(a a a a b c c a a d e e e e))
//((A A A A) (B) (C C) (A A) (D) (E E E E))

                 
let packFunction (prev, current, result) item = if item = prev then (prev, item::current, result) else (item, [item], current::result)

let pack l = 
            let (_,b,a) = List.fold packFunction (' ',[], []) l
            b::a |> List.rev |> List.tail
               

let res9 = items |> pack



//Run-length encoding of a list.
//Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
//
//Example:
//* (encode '(a a a a b c c a a d e e e e))
//((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

let numEncode (prev, current, result) item = if item = prev then (prev, current + 1, result) else (item, 1, (current,prev)::result)
let rec numEnc l = 
                let (c,b,a)  = List.fold numEncode (' ',1, []) l
                (b, c)::a |> List.rev |> List.tail
               

let res10 = items |> numEnc


// Modified run-length encoding.
//Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
//
//Example:
//* (encode-modified '(a a a a b c c a a d e e e e))
//((4 A) B (2 C) (2 A) D (4 E))
//P12 (**) Decode a run-length encoded list


type numEncodeItem<'T> = 
            | C of int * 'T
            | V of 'T 
let numenc2 (c,i) = if c <> 1 then C (c,i) else V i

let numEnc2 l = List.map numenc2 l

let res11 = res10 |> numEnc2



//Decode a run-length encoded list.
//Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

let createRep it = match it with 
                        | V a -> [a]
                        | C (b,c) -> [for i in 1..b -> c]
let deCompress l = List.collect createRep l

let res12 = res11 |> deCompress



//P13 (**) Run-length encoding of a list (direct solution).
//Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
//
//Example:
//* (encode-direct '(a a a a b c c a a d e e e e))
//((4 A) B (2 C) (2 A) D (4 E))

//
// Duplicate the elements of a list.
//Example:
//* (dupli '(a b c c d))
//(A A B B C C C C D D)

let duplicate l = List.collect (fun a-> [a; a]) l 
let res14 = items |> duplicate

//Replicate the elements of a list a given number of times.
//Example:
//* (repli '(a b c) 3)
//(A A A B B B C C C)


let dubrep c l = List.collect (fun a-> [for i in 1..c -> a]) l
let res15 = items |> dubrep 3



//Drop every N'th element from a list.
//Example:
//* (drop '(a b c d e f g h i k) 3)
//(A B D E G H K)

let createfilterNth() = 
                    let pos = ref 3     
                    let curr = ref 0          
                    (fun (newPos) -> pos := newPos),
                    (fun () -> curr := 0),
                    (fun _ ->
                                curr := (curr.Value + 1)
                                if !curr = !pos then 
                                                     curr := 0 
                                                     false else true)

let ChangeValue, Init , FilterNth = createfilterNth()
//combine changevalue and init 

let alpha = [1..10] 

let res16 = 
            let _ = Init()
            let _ = ChangeValue (4)
            alpha |> List.filter FilterNth


// Split a list into two parts; the length of the first part is given.
//Do not use any predefined predicates.
//
//Example:
//* (split '(a b c d e f g h i k) 3)
//( (A B C) (D E F G H I K))



let rec splitAtPos pos current l = let a, b =  if pos = current then ([] , List.tail l)    
                                                 else  splitAtPos pos (current + 1) (List.tail l)
                                   List.head l :: a , b
let SplitListAt list at = splitAtPos at 1 list
                                  
let res17 = SplitListAt items 7



//P18 (**) Extract a slice from a list.
//Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
//
//Example:
//* (slice '(a b c d e f g h i k) 3 7)
//(C D E F G)


let doubleSplit start endd list =   let a ,b = SplitListAt list (start - 1)
                                    let c ,d = SplitListAt b (endd - start + 1)
                                    c
let res18 = doubleSplit 2 9 items

//P19 (**) Rotate a list N places to the left.
//Examples:
//* (rotate '(a b c d e f g h) 3)
//(D E F G H A B C)
//
//* (rotate '(a b c d e f g h) -2)
//(G H A B C D E F)
//
//Hint: Use the predefined functions length and append, as well as the result of problem P17.

let rotate positions list = let rot = if positions > 0 then positions else (List.length list) + positions                              
                            let a,b = SplitListAt list rot
                            b @ a

let res19 = rotate 3 items
let res19b = rotate -3 items

//P20 (*) Remove the K'th element from a list.
//Example:
//* (remove-at '(a b c d) 2)
//(A C D)


let removek k list = let a, b = SplitListAt list k
                     let headminusone = a |> List.rev |> List.tail |> List.rev 
                     headminusone @ b 

let res20 = removek 10 items



//P21 (*) Insert an element at a given position into a list.
//Example:
//* (insert-at 'alfa '(a b c d) 2)
//(A ALFA B C D)

let insertat value pos list = let a,b = SplitListAt list (pos-1) 
                              a |> List.rev |> (fun c d -> c ::d ) value |> List.rev |> (fun c d -> d @ c) b


let res21 = insertat 'z' 4 items



//P22 (*) Create a list containing all integers within a given range.
//If first argument is smaller than second, produce a list in decreasing order.
//Example:
//* (range 4 9)
//(4 5 6 7 8 9)


let GenerateList a b = [a..b]
let res22 = GenerateList 5 20





//P23 (**) Extract a given number of randomly selected elements from a list.
//The selected items shall be returned in a list.
//Example:
//* (rnd-select '(a b c d e f g h) 3)
//(E D A)

open System
let rng = new Random()

let randomSelect list toget = 
  let arr = list |> Array.ofList
  [ 1 .. toget ] |> List.map (fun _ -> arr.[rng.Next(arr.Length)] )

let res23 = randomSelect items 3
  
                                           
//P24 (*) Lotto: Draw N different random numbers from the set 1..M.
//The selected numbers shall be returned in a list.
//Example:
//* (lotto-select 6 49)
//(23 1 17 33 21 37)
//
//Hint: Combine the solutions of problems P22 and P23.                         
                            
let LottoDraw draw range = randomSelect [1..range] draw

let res24 = LottoDraw 5 50
            
     
//
//P25 (*) Generate a random permutation of the elements of a list.
//Example:
//* (rnd-permu '(a b c d e f))
//(B A D C E F)
//
//Hint: Use the solution of problem P23.


let ReorderList list =                 
                let rec RandomOrder data = seq { if Array.length data > 0 then 
                                                         let pos = rng.Next(data.Length)
                                                         yield  data.[pos] 
                                                         data.[pos] <- None
                                                         let filtered d = Array.filter (fun i -> if Option.isSome i then true else false) d
                                                         yield! data |> filtered |> RandomOrder
                                                    }
                list |> List.map (fun x -> Some x) |> List.toArray 
                |>  RandomOrder 
                |> Seq.toList |> List.map(fun x -> Option.get x )
                        
let res25 = ReorderList items



//P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
//In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities 
//(C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
//
//Example:
//* (combination 3 '(a b c d e f))
//((A B C) (A B D) (A B E) ... )

//let combine a bs = List.map (fun b -> b::a) bs
//let combine2 list = let items = List.toArray list
  //                  List.collect (fun k -> combine k list) list 






//P27 (**) Group the elements of a set into disjoint subsets.
//a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
//
//Example:
//* (group3 '(aldo beat carla david evi flip gary hugo ida))
//( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
//... )
//
//b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
//
//Example:
//* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
//( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
//... )
//
//Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
//
//You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".




//P28 (**) Sorting a list of lists according to length of sublists
//a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
//
//Example:
//* (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
//((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
//
//b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
//
//Example:
//* (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
//((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
//
//Note that in the above example, the first two lists in the result have length 4 and 1, both lengths appear just once. The third and forth list have length 3 which appears twice (there are two list of this length). And finally, the last three lists have length 2. This is the most frequent length.
//
//
//
//http://www.facebook.com/pages/%CE%9A%CE%A5%CE%9D%CE%9F%CE%94%CE%9F%CE%9D%CE%A4%CE%91%CE%A3-DOGTOOTH/200050540288