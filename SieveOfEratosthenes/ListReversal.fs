module ListReversal

//////////////////////////////
// REVERSING A LINKED LIST //
////////////////////////////

let reverseList input = 
    let rec reverseList' aList cont = 
        match aList with
        | [] -> []
        | hd::[] -> hd::cont()
        | hd::tail -> 
            reverseList' tail (fun () -> hd::cont())
    reverseList' input (fun () -> [])