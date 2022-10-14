//21.
let curry f = fun (x: int) -> (fun (y: int) ->( f(x, y): int))

let uncurry g = fun(x: int, y: int) -> (g x y): int
