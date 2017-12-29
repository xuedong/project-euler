let even_sum limit =
  let t1 = ref 0 in
  let t2 = ref 2 in
  let sum = ref 2 in
  if (limit < 2) then
    sum := 0
  else
    while (!t2 <= limit) && (4 * !t2 + !t1 <= limit) do
      let t3 = 4 * !t2 + !t1 in
      t1 := !t2;
      t2 := t3;
      sum := !sum + !t2  
    done;
  !sum;;

even_sum 4000000
