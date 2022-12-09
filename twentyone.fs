//Определите последовательность факториалов неотрицательных целых чисел 1,1,2,6,...,n!
// 50.2.1

let rec fact = function
    | 0 -> 1
    | n -> n * fact (n-1)

let fac_seq = seq {
    for i in 0..10 do yield fact i
}


//Определите последовательность 0, -1, 1, -2, 2, -3, 3, ...
// 50.2.2
let seq_seq = seq {
    yield 0
    for i in 1..10 do 
    yield! seq [-i; i]
}
