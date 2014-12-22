module NeighborExtraction

///////////////////////////////////
// FINDING NEIGHBOURS IN A GRID  //
///////////////////////////////////
open System

let getNeighbours (x,y) (matrix: 'a [,]) = 
    let lower n = max 0 (n - 1)
    let upper n = min (matrix.GetUpperBound(0)) (n + 1)
    seq { for i in lower x..upper x do
            for j in lower y..upper y do
             if (i,j) <> (x,y) then yield matrix.[i, j] }
    
// Tests
let rnd = Random()
let arr = Array2D.init 8 8 (fun _ _ -> rnd.Next(100))
let slice = getNeighbours (3, 4) arr