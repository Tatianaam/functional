// // let vat n x = x * (1.0 + (float n) / 100.0) 

// // let pi = 3.14159
// // let circle r = 2.0 * pi * r 
// // printfn "%f" (circle 2.0) // 12.566360




// let unvat n x = x / (1.0 + (float n) / 100.0)


// let a = fun y -> y - 5
// let b = fun z -> z * z * z
// let c = a << b
// printfn "%d" (c 2) // 2*2*2 - 5 = 3
// ///let h = a >> b // h 2 = b( a(2) ) = b( -3 ) = -27 


// let f x = fun y -> y - x
// let minus10 = f 10
// printfn "%d" (f 2 3) // +1
// printfn "%d" (f 10 12) // +2
// printfn "%d" (minus10 12) // +2
// let h5 = ((fun y -> y - 5) << (fun z -> z * z * z)) 2 // 3
// //21.1
//curry: (int * int -> int) -> int -> int ->int
//curry f -- это функция g, где g x -- это функция h, где h y = f(x,y).
//uncurry: (int -> int ->int) -> int * int -> int
//uncurry g -- это функция f, где f(x,y) -- это значение h y для функции h = g x.
let test (x, y) = x * y
let compose4 op1 op2 n = op1 (op2 n)
let compose4curried =
    fun op1 ->
        fun op2 ->
            fun n -> op1 (op2 n)


let curry f = fun (x: int) -> (fun (y: int) ->( f(x, y): int))
let uncurry g = fun(x, y) -> f g


// let curry f =
//     fun (x: int) ->
//         fun (y: int)->
//             fun f -> f(x, y): int



// let h y = fun x -> f x y
// let g x = fun y -> h y
// let curry f = h f >> g 


// let h n (y: int) = fun (x: int) -> n(y, x): int
// let g f x = fun (y: int) -> h f y
// let curry1 f = 
//     let x = 2
//     g f x

