//Напишите функцию факториала f: int -> int, не используя рекурсию, с помощью императивных возможностей
// 47.4.1
let f n : int =
    let x = ref (n-1)
    let mutable res = n
    while !x > 0 do
        res <- res * !x
        x := !x - 1
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