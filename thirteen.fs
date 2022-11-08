// 39.1
let rec help nums even = 
    match nums with
    | [] -> []
    | head :: tail when even -> help tail (not even)
    | head :: tail -> head :: help tail (not even) 

let rec rmodd nums = help nums true 

// 39.2
let rec del_even nums = nums |> List.filter (fun x -> x % 2 <> 0)

// 39.3
let rec multiplicity x xs = 
    match xs with 
    | [] -> 0
    | head :: tail when x = head -> 1 + multiplicity x tail
    | head :: tail -> multiplicity x tail


// 39.4
let rec split nums = help nums false , help nums true

// 39.5
exception NotEqualLength

let rec zip (xs1, xs2) = 
    if (xs1 |> List.length) <> (xs2 |> List.length) then raise NotEqualLength 
    else (xs1, xs2) ||> List.map2 (fun s1 s2 -> (s1, s2))
