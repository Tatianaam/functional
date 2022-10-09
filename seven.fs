// 20.3.1
let vat (n :int, x) = x + x * float n / 100.0 

// 20.3.2
let unvat (n : int, x) = x * 100.0 / (100.0 + float n)

//20.3.3.
let rec help3(f, n) = if f n = 0 then n else help3(f, n+1)

let rec min f =
    let n = 1
    if f n = 0 then n else help3(f, n+1)
