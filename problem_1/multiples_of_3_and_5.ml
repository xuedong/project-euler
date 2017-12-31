(* Linear complexity *)
let sum_multiples_1 n =
  let sum = ref 0 in
  for i = 3 to (n-1) do
    if (i mod 3) == 0 || (i mod 5) == 0 then
      sum := !sum + i;
  done;
  !sum

(* Constant complexity *)
let sum_multiples_2 n =
    let sum3 = ((n-1)/3) * (3 * ((n-1)/3) + 3) / 2 in
    let sum5 = ((n-1)/5) * (5 * ((n-1)/5) + 5) / 2 in
    let sum15 = ((n-1)/15) * (15 * ((n-1)/15) + 15) / 2 in
    sum3 + sum5 - sum15;;

sum_multiples_2 1000;;
