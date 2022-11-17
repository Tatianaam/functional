// 41.4.1.
let list_filter f xs = List.foldBack (fun cur prev -> if f cur then cur :: prev else prev) xs []

// 41.4.2
let sum (p, xs) = List.foldBack (fun cur prev -> if p cur then cur+prev else prev) xs 0

// 41.4.3
let rev lst  = List.fold(fun head tail -> tail :: head) [] lst

let revrev = fun x -> List.fold(fun head tail -> rev tail :: head) [] x



//let testRev lst = List.foldBack(fun head tail -> tail @ [head]) lst []
//let testRevRev lst = List.foldBack(fun head tail -> tail @ [(List.fold(fun head tail -> tail :: head) [] head)] ) lst []
