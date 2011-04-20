// Learn more about F# at http://fsharp.net

//Last item in a list 
let theList = [1..5]

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

let items = ['a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'a'; 'a'; 'c'; 'd'; 'd'; 'e'; 'e';] 
                           
let compress input = 
                    let removeRepetions (out, rep) l = List.fold (fun (out, rep) l -> if l = rep then (out, l) else (l :: out, l)) (out,rep) l
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



