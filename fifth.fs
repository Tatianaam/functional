// 16.1 
let notDivisible = function
| (0, m) -> false
| (n, m) when m % n = 0 -> true
| _ -> false

// 16.2
let rec div  = function
| (n, 1) -> true
| (n, m) when n % m = 0 -> false
| (n, m) -> div(n, m-1)

let prime = function
| 0 | 1 -> false
| n -> div (n, n-1)