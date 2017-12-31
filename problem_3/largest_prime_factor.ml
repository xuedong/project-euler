(* Naive version *)
let largest_factor_1 n =
  let rec aux_factor d n =
    if (d >= n) then
      n
    else if (n mod d == 0) then
      aux_factor (d+1) (n/d)
    else
      aux_factor (d+1) n
  in
  aux_factor 2 n;;

(* Robust version *)
let largest_factor_2 n =
  let rest = ref n in
  let last_factor = ref 1 in
  let factor = ref 3 in
  if (!rest mod 2 = 0) then
    begin
      last_factor := 2;
      rest := !rest / 2;
      while (!rest mod 2 = 0) do
        rest := !rest / 2
      done
    end;
  let max_factor = ref (int_of_float(sqrt(float_of_int(!rest))) + 1) in
  while (!rest > 1) && (!factor < !max_factor) do
    if (!rest mod !factor = 0) then
      begin
        rest := !rest / !factor;
        last_factor := !factor;
        while (!rest mod !factor = 0) do
          rest := !rest / !factor
        done;
        max_factor := int_of_float(sqrt(float_of_int(!rest)))
      end;
    factor := !factor + 2
  done;
  if (!rest = 1) then
    !last_factor
  else
    !rest;;

largest_factor_2 600851475143;;
