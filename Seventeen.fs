// 43.3
let try_find key m = 
    let rec tFind(key, m :('b * 'c) list) = 
        match m with
        | [] -> None
        | (a, b) :: tail when a = key -> Some(b)
        | head :: tail -> tFind(key, tail)
    tFind(key, Map.toList m)
