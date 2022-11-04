let rec help nums even = 
    match nums with
    | [] -> []
    | head :: tail when even -> help tail (not even)
    | head :: tail -> head :: help tail (not even) 

// 39.1
let rec rmodd nums = help nums true 


let isUnEven (i, _) = i % 2 <> 0

// 39.2
let rec del_even nums = nums |> List.indexed |> List.filter isUnEven |> List.map (fun (i, n) -> n)

// 39.3
let rec multiplicity x xs = 
    match x with 
    | [] -> 0
    | head :: tail when xs = head -> 1 + multiplicity tail xs
    | head :: tail -> 0 + multiplicity tail xs


// 39.4
let rec split nums = (help nums false , help nums true)

exception NotEqualLength

// 39.5
let rec zip (xs1, xs2) = 
    if (xs1 |> List.length) <> (xs2 |> List.length) then raise NotEqualLength 
    else (xs1, xs2) ||> List.map2 (fun s1 s2 -> (s1, s2))

