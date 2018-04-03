
fun add3 x = x + 3;

fun fact x = 
  if x = 0
  then 1
  else x * fact (x - 1);

fun fact_tail t = case t of
                       (0, ans) => ans
                    |  (x, ans) => fact_tail (x-1, ans * x);

(* => is the evaluation sign. Telling you matched results
*
* In the first part, the pattern
* that the first is 0 will be matched. ans, a variable name,
* binds the second part of the tuple.*)

fun fact_tail2 (0, ans) = ans
  | fact_tail2 (x, ans) = fact_tail2(x-1, ans * x);

(* Let *)

fun fact_tail3 x =
    let fun help (0, ans) = ans
  | help (x, ans) = help(x-1, x * ans)
    in 
help(x, 1)
  end;


(* Record: Similar to Dictionary in Python *)


fun max gt =
    let fun lp curr [] = curr
          | lp curr (a::l) = if gt(a, curr)
                             then lp a l  # this
                             else lp curr l
    in
        lp
    end;
(* Here, gt is a function. It can be val maxInt = max (op >) if
 we want to do integer comparison.
 *)

fun max gt [] = NONE
  | max gt (a::l) =
    let fun lp curr [] = curr
          | lp curr (a::l) = if gt(a, curr)
                             then lp a l
                             else lp curr l
    in
        SOME(lp a l)
    end;

(*Given a function, ruturn another function!
                         (’a * ’a -> bool) -> (’a list -> ’a option)*)

val maxInt = max (op >)
val weirdMaxInt =
    max (fn (x, y) =>
            if x mod 2 <> y mod 2
            then x mod 2 = 0
            else x > y);

(* fn is the keyword like lambda in Python *)

(* map
  (`a -> `b) -> `a list -> `b list

For example, map (fn x => x-1)
 *)

(* filter
  (`a -> bool) -> `a list -> `a list
*)

(* redute

*)

(* fold
First, recall that
The type is reported as int -> int (read “int to int”).
The arrow indicates a function type.
The type on the left side of the arrow is the argument type,
and the type on the right side of the arrow is the return type.

 (`a * `b -> `b) -> `b -> `a list -> `b

*)
(*


 Note that foldl, foldr, and map are all available directly in the top level
environment (you can just use their names and get the right function). filter is not,
but is accessible as List.filter (moreon structures later).
*)

let val r = ref 0
    val x = r
    val () = r:= 42 (* changes the value referenced by r to be 42 *)
in
    !x
end;

(* recall that unit is the name for a zero-element tuple and is the type for
things that produce no meaningful value, but are done for side-effect
 *)

fun makeCounter initVal =
    let val r = ref initVal
    in
        fn () => let val ans = !r
                     val () = r := ans + 1
                 in
                     ans
                 end
    end

 (* what is the sequence of execution of let val ans ... ? *)

        (* The value restriction *)

(* https://stackoverflow.com/questions/49087750/writing-power-function-in-standard-ml-with-a-predefined-compound-function?noredirect=1#comment85181581_49087750 *)

fun compound 0 f x = x 
  | compound n f x = compound (n-1) f (f x)


fun exp b 0 = 1  
  | exp b e = 
    let fun f p x = p * x 
    in 
      compound e (f b) 1  
    end

fun exp b 0 = 1  
  | exp b e = compound e (fn x => b*x) 1

fun exp b 0 = 1
  | exp b e = case compound e (fn (b, x) => (b, x*b)) of
                   (b, ans) => ans

(*https://stackoverflow.com/questions/49102648/making-a-tree-code-tail-recursive-in-sml-nj
Making a tree code tail recursive in SML/NJ
 *)
datatype 'a tree =
         leaf of 'a
       | node of 'a tree * 'a tree

fun catTreverse (leaf l) = l
  | catTreverse n =
    let fun helper (nodeList, leaves) =
            case nodeList of
                nil => leaves
              | x::xs => case x of
                              leaf (l) => helper(xs, l::leaves)
                            | node(a,b) => helper(a::b::xs, leaves)
    in
        String.concat (List.rev (helper (n::nil, [])))
    end

fun cat (leaf l) = l
  | cat (node n) =
    let fun helper (nodeList, leaves) =
            case nodeList of
                nil => leaves
              | x::xs => case x of
                             leaf (l) => helper(xs, l::leaves)
                           | node(a,b) => helper(a::b::xs, leaves)
    in
        String.concatWith " " (List.rev (helper ([node(n)], [])))
    end
