let is_palindrome n =
  let s = string_of_int n in
  let l = String.length s in
  let rec aux n =
    (n = 0) || (s.[l-n] = s.[n-1] && aux (n-1)) in
  aux (l/2)

let largest_product limit =
  let current_max = ref 0 in
  for i = 1 to limit do
    for j = i to limit do
      let prod = i*j in
      if (prod > !current_max) && (is_palindrome prod) then
        current_max := prod
    done
  done;
  !current_max;;      

largest_product 999;;
