(* Range from 0 to n. *)
let range n =
  let rec range_ n acc =
    if n = -1 then
      acc
    else 
      range_ (n-1) (n::acc)
  in range_ n [];;

(* Fixed point operator. *)
let rec fix f x = f (fix f) x;;

(* Create a list having element a repeated n times. *)
let repeat a n =
  let rec repeat_ a n acc =
    if n == 0 then 
      acc
    else
      repeat_ a (n-1) (a::acc)
  in repeat_ a n [];;
