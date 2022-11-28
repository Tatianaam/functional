// 47.4.1
let f n =
    let x = ref (n-1)
    let mutable res: int = n
    while x.Value > 0 do
        res <- res * x.Value
        x.Value <- x.Value - 1
    res


// 47.4.2
let fibo n = 
    if n < 2 then n else
        let arr = [|0 .. n|]
        let mutable i = 2
        while i <= n do
            arr.[i] <- arr[i-2] + arr[i-1]
            i <- i + 1
        arr.[n]