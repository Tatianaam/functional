// 20.3.1
let h1  = fun (x: int) -> 0.01 * float x
let vat = fun n x-> x + x * h1 n

// 20.3.2
let h2 = fun (x: int) -> 100.0 + float x
let unvat = fun n x -> 100.0 * x / h2 n

//20.3.3.
let rec help3(f, n) = if f n = 0 then n else help3(f, n+1)

let rec min f =
    let n = 1
    if f n = 0 then n else help3(f, n+1)

    