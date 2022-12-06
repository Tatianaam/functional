//Определите последовательность чётных положительных чисел.
// 49.5.1
let even_seq =  Seq.initInfinite (fun i -> (i+1)*2)


//Определите последовательность факториалов неотрицательных целых чисел 1,1,2,6,...,n!
// 49.5.2
let fac_seq =Seq.initInfinite (fun i -> 
    let rec fctr = function
        | 0 -> 1
        | n -> n * fctr(n-1) 
    fctr i)

//                               0  1   2   3  4   5  6
// Определите последовательность 0, -1, 1, -2, 2, -3, 3, ...
// 49.5.3
let seq_seq = Seq.initInfinite (fun i -> if i % 2 = 0 then i/2 else -1 * (i+1)/2)
