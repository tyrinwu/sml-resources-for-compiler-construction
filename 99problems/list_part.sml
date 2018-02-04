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

(*P06  Find out whether a list is a palindrome.*)

fun isPalindrome l =
    let fun splitList l =
            let val sz = List.length l;
                fun helper (x, a::l, n1, n2) = case n1 - n2 of
                                                  0 => (x, a::l)
                                                | 1 => (x, l)
                                                | _ => helper(a::x, l, n1-1, n2+1)
            in
                helper([], l, sz, 0)
            end
        fun cmp (x::l, y::l2) = if x = y then cmp (l, l2) else false
          | cmp ([], []) = true
    in
        cmp (splitList(l))
    end

(*P07 Flatten a nested list structure.*)
