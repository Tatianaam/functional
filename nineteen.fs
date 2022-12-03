// 48.4.1
let rec fibo1 n n1 n2 = 
    if n = 1 then n1 
    else fibo1 (n-1) (n1 + n2) n1


// 48.4.2
let rec fibo2 n c = 
    if n < 2 then n else
    let rec help (n: int) (c: int -> int) (a: int) (b: int) =
        if n = 1 then c b
        else help (n-1) (fun a -> c a) b (a+b)
    help n c 0 1
    
    
// 48.4.3
let rec bigList n k =
    let rec bF n k l = 
        if n=0 then l
        else bF (n-1) (fun res -> res) (1::l)
    bF n k []
