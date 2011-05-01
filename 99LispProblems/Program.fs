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



let rec splitAtPos pos current l = 
                                     let a, b =  if pos = current then ([] , List.tail l)    
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


let doubleSplit start endd list = 
                                    let a ,b = SplitListAt list (start - 1)
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
