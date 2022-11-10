//40.1. Напишите функцию sum(p, xs), 
//где p -- предикат int -> bool, и xs -- список целых. 
//Функция возвращает сумму тех элементов xs, для которых предикат истинен.

// 40.1
let rec sum (p, xs: int list) = 
    match xs with   
    | [] -> 0
    | head :: tail when p head -> head + sum (p, tail)
    | head :: tail -> sum (p, tail)


//40.2. Список [x1; x2; ...; xn] называется слабо восходящим, если его элементы удовлетворяют требованию
// x1 <= x2 <= ... <= xn
//Напишите функцию count: int list * int -> int, которая подсчитывает количество вхождений числа в список.

// 40.2.1
let rec count (xs: int list, n: int) =
    match xs with
    | [] -> 0
    | head :: tail when head = n -> 1 + count(tail, n)
    | head :: tail -> count(tail, n)


//40.2.2. Напишите функцию insert: int list * int -> int list, которая добавляет новый элемент в список.
// 40.2.2
let rec insert (xs: int list, n: int) =
    if xs.IsEmpty then [n]
    elif xs.Head >= n then n :: xs
    else xs.Head :: insert (xs.Tail, n)


//40.2.3. Напишите функцию intersect: int list * int list -> int list,
// которая находит общие элементы в обоих списках, включая повторяющиеся.
// 40.2.3
let rec intersect (xs1: int list, xs2: int list) =
    match xs1 with 
    | [] -> []
    | _ when xs2.IsEmpty -> []
    | head :: tail when head = xs2.Head -> [head] @ intersect(tail, xs2.Tail)
    | head :: tail when head < xs2.Head -> intersect(tail, xs2)
    | head :: tail -> intersect(xs1, xs2.Tail)


//40.2.4. Напишите функцию plus: int list * int list -> int list, 
//которая формирует список, объединяющий все элементы входных списков, включая повторяющиеся.
//// 40.2.4
let rec plus (xs1: int list, xs2: int list) = 
    match xs1 with 
    | _ when xs2.IsEmpty -> xs1
    | _ when xs1.IsEmpty -> xs2
    | head :: tail when head <= xs2.Head -> [head] @ plus(tail, xs2)
    | _ -> [xs2.Head] @ plus(xs1, xs2.Tail)


//40.2.5. Напишите функцию minus: int list * int list -> int list, 
//которая возвращает список, содержащий элементы первого списка
// за исключением элементов второго списка (элементы, одинаковые по значению, считаются разными).
// 40.2.5
let rec minus (xs1: int list, xs2: int list) =
    match xs1 with
    | [] -> []
    | _ when xs2.IsEmpty -> xs1
    | head :: tail when head = xs2.Head -> minus(tail, xs2.Tail)
    | head :: tail when head < xs2.Head -> [head] @ minus(tail, xs2)
    | head :: tail -> minus(xs1, xs2.Tail)

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
let rec delete (n: int, xs: int list) =
    match xs with
    | [] -> xs
    | head :: tail when xs.Head = n -> tail 
    | head :: tail -> head :: delete(n, tail)


//40.3.3. Напишите функцию сортировки с использованием предыдущих функций, 
//которая сортирует входной список так, что на выходе получается слабо восходящий список.
// 40.3.3
let rec sort elms =
    match elms with
    | [] -> []
    | elms -> smallest elms :: sort(delete(smallest elms, elms))


//40.4. Напишите функцию revrev, которая получает на вход список списков,
// и перевёртывает как порядок вложенных списков, так и порядок элементов внутри каждого вложенного списка.
//revrev [[1;2];[3;4;5]] = [[5;4;3];[2;1]]
// 40.4
let rec revrev outs  =
    let rec rev ins =
        match ins with
        | [] -> []
        | head :: tail -> rev tail @ [head]
    match outs with
    | [] -> []
    | head :: tail -> revrev tail @ [rev head]
