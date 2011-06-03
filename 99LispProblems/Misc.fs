module Misc

//For fun random generator 

open System 
let rng = new Random()
let rec RandomNumbers range = seq { 
                                yield rng.Next(range) 
                                yield! RandomNumbers range }                                     
                           
      