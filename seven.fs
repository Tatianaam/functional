// 20.3.1
let help = function y -> y/100.0

let vat (n: int) x  = x + x * help n 

// 20.3.2
let unvat n x =vat n x - x * help n

//20.3.3.
let rec help3(f, n) = if f n = 0 then n else help3(f, n+1)

let rec min f =
    let n = 1
    if f n = 0 then n else help3(f, n+1)
