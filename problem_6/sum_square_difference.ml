let sum_square n =
  let sum = ref 0 in
  for i = 1 to n do
    sum := !sum + i * i
  done;
  !sum

let square_sum n =
  let sum = ref 0 in
  for i = 1 to n do
    sum := !sum + i
  done;
  (!sum) * (!sum);;

(square_sum 100) - (sum_square 100);;
