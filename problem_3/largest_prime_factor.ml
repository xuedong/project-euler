let largest_factor n =
  let rec aux_factor d n =
    if (d >= n) then
      n
    else if (n mod d == 0) then
      aux_factor (d+1) (n/d)
    else
      aux_factor (d+1) n
  in
  aux_factor 2 n;;

largest_factor 600851475143;;
