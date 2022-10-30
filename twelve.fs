// 34.1
let rec helper = function
| (c, n, l) when c = n -> helper(c-1, n , [n]) 
| (c, n, l) when c = 0 -> l
| (c, n, l) -> helper(c-1, n, c :: l) 

let rec upto n = if n <= 0 then [] else helper(n, n, [])

//34.2
let rec helperD = function
| (c, n, l) when c = n -> c :: l
| (c, n, l) when c = 1 -> helperD(c+1, n , [1]) 
| (c, n, l) -> helperD(c+1, n, c :: l) 


let rec dnto n = if n <= 0 then [] else helperD(1, n, [])

// 34.3
let rec helperE = function
| (c, n, l) when c = 0 -> 0 :: l
| (c, n, l) when c = n -> helperE(c-2, n, c :: l)
| (c, n, l) -> helperE(c-2, n, c :: l)

let rec evenn n = if n <= 0 then [] else helperE (n*2-2, n*2-2, [])