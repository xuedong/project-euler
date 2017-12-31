let is_prime n =
  if n = 2 then
    true
  else if (n < 2) || (n mod 2 = 0) then
    false
  else
    let rec aux k =
      if (k * k > n) then
        true
      else if (n mod k = 0) then
        false
      else
        aux (k+2)
    in aux 3

let nst_prime n =
  let k = ref 3 in
  let i = ref 1 in
  while (!i < n) do
    if (is_prime (!k)) then
      begin
        i := !i + 1;
        k := !k + 2
      end
    else
      k := !k + 2
  done;
  !k-2;;

nst_prime 10001;;
