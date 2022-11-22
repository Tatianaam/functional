// Напишите функцию allSubsets, получающую целочисленные параметры n и k, 
// и выдающую множество всех подмножеств множества {1, 2, ..., n},
// в которых ровно k элементов (0 <= k <= n).

// Шаблон для отправки на сервер:

// 42.3

let iterateReplace (s: Set<int>, k : int) =
 [for x in s -> Set.ofList (k :: (List.filter (fun y -> y <> x) (Set.toList s)))]


let rec addSubsets =  function 
    | (n : int, k : int, sets: Set<int> list) when n = k -> 
        let lst: Set<int> list = List.Empty
        List.fold(fun acc cur-> acc @ iterateReplace(cur, k)) sets sets
    | (n: int, k: int, sets: Set<int> list) -> 
        let lst = List.fold(fun acc cur -> acc @ iterateReplace(cur, k)) [] sets
        sets @ lst @ addSubsets(n, k+1, lst)


let rec allSubsets n k = 
    if n = 0 || k = 0 then Set.empty
    elif n = k then Set.empty.Add(Set.ofList [ for i in 1 .. n -> i ])
    elif k = 1 then
        let lst = [ for i in 1 .. n -> Set.ofList [i] ]
        Set.ofList lst
    else 
        let temp = Set.ofList [ for i in 1 .. k -> i ]
        Set.ofList(addSubsets(n, k+1, temp :: List.Empty)).Add(temp)