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
        let mutable first = 0
        let mutable second = 1
        let mutable count = 2
        while count <= n do
            let next = first + second
            first <- second
            second <- next
            count <- count + 1
        second