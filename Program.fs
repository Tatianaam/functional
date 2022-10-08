//20.3.3. Напишите функцию min: (int -> int) -> int, так что 
//min(f) вычисляет минимальное целое положительное число n,
// когда f(n) = 0. Подразумевается, что такое натуральное число существует всегда.
//20.3.3.
let rec help3(f, n) = if f n = 0 then n else help3(f, n+1)

let rec min f =
    let n = 1
    if f n = 0 then n else help3(f, n+1)





printfn "%d" (help3( y, 2)) 