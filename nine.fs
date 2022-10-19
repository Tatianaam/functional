// 23.4.1
let toCopper ((a,b,c), (n, m, l)) = c + l + 12 * (b + m + 20 * (a + n))

let toSilver (a, b, c) = (a, b + c/12, c % 12)

let toGold (a, b, c) = (a + b/20, b%20, c)

let negative (x, y, z) = (-x, -y, -z)

let (.+.) x y = (0, 0, toCopper (x, y)) |> toSilver |> toGold

let (.-.) x y = x .+. negative y


// 23.4.2
let negTwo (a, b) = (-a, -b)
let divTwo (a, b) = (a/(a*a+b*b), -b/(a*a+b*b))

let (.+) x y = fun ((a,b), (c,d))->  (a + c, b + d)

let (.-) x y = x .+ negTwo y

let (.*) x y =  fun ((a,b), (c,d))->  (a*c - b*d, b*c + a*d)

let (./) x y = x .* divTwo y
