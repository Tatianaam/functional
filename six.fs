// 17.1
let rec pow = function
| (s,  0) -> ""
| (s, n) when n < 0 -> ""
| (s, n) -> s + pow(s, n-1)

//17.2
let rec isIthChar = function 
| (s, n, c) when n < 0 || n >= String.length(s) || s.[n] <> c -> false
| (s, n, c) -> true

//17.3
let rec occFromIth = function
| (s, n, c)  when n < 0 || n >= String.length(s) -> 0
| (s, n, c) when s.[n] <> c -> 0 + occFromIth(s, n + 1, c)
| (s, n, c) -> 1 + occFromIth(s, n + 1, c)
