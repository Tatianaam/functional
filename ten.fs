//24
type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y = if x.f < y.f then false elif x.f > y.f then true elif x.f = y.f && x.hours * 60 + x.minutes > y.hours * 60 + y.minutes then true else false