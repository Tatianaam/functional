type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let transform = function
| x when x.f = AM -> 60*x.hours + x.minutes
| x ->  60*(x.hours+12) + x.minutes

let (.>.) x y = (transform x) > (transform y)
