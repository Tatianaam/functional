//Определите последовательность факториалов неотрицательных целых чисел 1,1,2,6,...,n!
// 50.2.1

let rec fact = function
    | 1 -> 1
    | n -> n * fact (n-1)

let fac_seq n = seq {
    for i in 1..n do yield fact i
}


//Определите последовательность 0, -1, 1, -2, 2, -3, 3, ...
// 50.2.2
let seq_seq n = seq {
    yield 0
    for i in 1..n do 
    yield! seq [-i; i]
}

