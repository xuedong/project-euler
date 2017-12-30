let rec gcd u v =
  if v <> 0 then
    (gcd v (u mod v))
  else
    (abs u)

let lcm u v = match u, v with
  | 0, _ | _, 0 -> 0
  | u, v -> abs (u * v) / (gcd u v)

let rec smallest_multiple n = match n with
  | 1 -> 1
  | n -> lcm (smallest_multiple (n-1)) n;;

smallest_multiple 20;;
