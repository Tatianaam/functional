//40.1. Напишите функцию sum(p, xs), 
//где p -- предикат int -> bool, и xs -- список целых. 
//Функция возвращает сумму тех элементов xs, для которых предикат истинен.

// 40.1
let rec sum (p, xs) = 
    match xs with   
    | [] -> 0
    | head :: tail when p head -> head + sum (p, tail)
    | head :: tail -> sum (p, tail)

//40.2. Список [x1; x2; ...; xn] называется слабо восходящим, если его элементы удовлетворяют требованию
// x1 <= x2 <= ... <= xn
//Напишите функцию count: int list * int -> int, которая подсчитывает количество вхождений числа в список.
// 40.2.1
let rec count (xs, n) =
    match xs with
    | [] -> 0
    | head :: tail when head = n -> 1 + count(tail, n)
    | head :: tail -> count(tail, n)


//40.2.2. Напишите функцию insert: int list * int -> int list, которая добавляет новый элемент в список.
// 40.2.2
let rec insert (xs, n) =
    match xs with
    |[] -> [n]
    |list -> list @ [n] 

//40.2.3. Напишите функцию intersect: int list * int list -> int list,
// которая находит общие элементы в обоих списках, включая повторяющиеся.
// 40.2.3
let rec intersect (xs1: 'a list, xs2: 'a list) =
    match xs1 with 
    | _ when xs2.IsEmpty || xs1.IsEmpty -> []
    | head :: tail when head = xs2.Head -> [head] @ intersect(tail, xs2.Tail)
    | head :: tail when head < xs2.Head -> intersect(tail, xs2)
    | head :: tail -> intersect(xs1, xs2.Tail)

//40.2.4. Напишите функцию plus: int list * int list -> int list, 
//которая формирует список, объединяющий все элементы входных списков, включая повторяющиеся.
//// 40.2.4
let rec plus (xs1: 'a list, xs2: 'a list) = 
    match xs1 with 
    | _ when xs2.IsEmpty -> xs1
    | _ when xs1.IsEmpty -> xs2
    | head :: tail when head <= xs2.Head -> [head] @ plus(tail, xs2)
    | _ -> [xs2.Head] @ plus(xs1, xs2.Tail)


//40.2.5. Напишите функцию minus: int list * int list -> int list, 
//которая возвращает список, содержащий элементы первого списка
// за исключением элементов второго списка (элементы, одинаковые по значению, считаются разными).
// 40.2.5
let rec minus (xs1: 'a list, xs2: 'a list) =
    match xs1 with
    | [] -> []
    | _ when xs2.IsEmpty -> xs1
    | head :: tail when head = xs2.Head -> minus(tail, xs2.Tail)
    | head :: tail when head < xs2.Head -> [head] @ minus(tail, xs2)
    | head :: tail when head > xs2.Head -> minus(xs1, xs2.Tail)

//40.3.1. Напишите функцию smallest: int list -> int option, 
//которая возвращает наименьший элемент непустого списка
// 40.3.1
let rec smallest (elms: int list) =
    let rec help (min : int, elms : int list) =
        match elms with
        | [] -> min
        | head :: tail when head < min -> help(head, tail)
        | _ -> help(min, elms.Tail)
    help(elms.Head, elms)

//40.3.2. Напишите функцию delete: int * int list -> int list, 
//которая удаляет из списка первое вхождение заданного элемента (если он имеется).
// 40.3.2
let rec delete (n, xs) =


let l1 = [0;0;7;1;2;9;123;3;6;8;9;-9;9]
let l2= [0;3;3;9]
let f x = x % 2 = 0
let a = smallest(l1)
printfn "%A" a 
