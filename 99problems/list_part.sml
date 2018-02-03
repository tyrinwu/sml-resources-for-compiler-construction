fun my_last l = List.last l

(*last l
  returns the last element of l. It raises Empty if l is nil. *)

(*pe·nul·ti·mate*)
fun last_but_one l = if List.length l <= 1
                     then NONE
                     else List.nth (l, List.length(l)-2)

(*P04 find the number of values in a list*)
fun numElements l =
let fun recur(num, x::left) = recur(num+1, left)
      | recur (num, NIL) = num
in
    recur(0, l)
end
(* Standard library: List.length l *)
    

(*P05 reverse a list*)
fun reverse l = 
    let fun helper (x::l, ans) = helper (l, x::ans)
          | helper (NIL, ans) = ans
    in
        helper (l, [])
    end
