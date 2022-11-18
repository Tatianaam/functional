// Напишите функцию allSubsets, получающую целочисленные параметры n и k, 
// и выдающую множество всех подмножеств множества {1, 2, ..., n},
// в которых ровно k элементов (0 <= k <= n).

// Шаблон для отправки на сервер:

// 42.3
let rec allSubsets n k =  
    if k = 1 then Set.ofList [ for i in 1 .. n -> Set.ofList [i] ]
    else Set.fold(fun cur (prev: Set<int>) -> cur.Add(prev.Add(n))) Set.empty (allSubsets (n-1) (k-1))

//    if k = 1 then Set.ofList [ for i in 1 .. n -> Set.ofList [i] ]
//    else 
//        let temp = (allSubsets (n-1) (k-1))
//        let additional = List.fold(fun cur prev -> cur.Add()) Set.empty temp
//        Set.fold(fun cur (prev: Set<int>) -> cur.Add(prev.Add(n))) Set.empty temp


let r = allSubsets 5 3
printfn"%A" r

// let n = 5
// let t = Set.ofList [ for i in 1 .. n -> Set.ofList [i] ]
// printfn"%A" t
// let s = Set.ofList [123]
// printfn"%A" s
// let t1 = t.Add(s)
// printfn"%A" t1

// let t2 = Set.fold(fun (cur: Set<Set<int>>) (prev: Set<int>) -> cur.Add(prev.Add(n))) Set.empty t1
// printfn"%A" t2