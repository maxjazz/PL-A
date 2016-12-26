(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu

fun g f1 f2 p =
  let 
	    val r = g f1 f2 
  in
	    case p of
	        Wildcard          => f1 ()
	      | Variable x        => f2 x
	      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	      | ConstructorP(_,p) => r p
	      | _                 => 0
  end

(* 1. Write a function only_capitals that takes a string list and returns a string list that has only the strings in the argument that start with an uppercase letter.*)
fun only_capitals (xs) =
  List.filter (fn x => Char.isUpper(String.sub(x,0))) xs;

(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the list. If the list is empty, return "" *)
fun longest_string1 (xs) =
  foldl (fn (x,y) => if String.size(x) > String.size (y)
                     then x
                     else y)
        "" xs;

(* 3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties it returns the string closest to the end of the list. *)
fun longest_string2 (xs) =
  foldl (fn(x,y) => if String.size(x) >= String.size(y)
                    then x
                    else y)
        "" xs;

(* 4 Write functions longest_string_helper, longest_string3, and longest_string *)
fun longest_string_helper f xs =
  foldl (fn (x,y) => if f(String.size x, String.size y)
                     then x
                     else y) "";

val longest_string3  = fn xs =>
  longest_string_helper (fn(x,y) => x > y) xs;

val longest_string4 = fn xs => 
  longest_string_helper (fn(x,y) => x >= y) xs;

(* 5. Write a function longest_capitalized that takes a string list and returns the longest string in the list that begins with an uppercase letter, or "" if there are no such strings.*)
val longest_capitalized  = longest_string1 o only_capitals;

(* 6. Write a function rev_string that takes a string and returns the string that is the same characters in reverse order. *)
val rev_string = String.implode o rev o String.explode;

(* 7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried).*)
fun first_answer _ [] = raise NoAnswer
  | first_answer f (x::xs) =
    case f(x)
     of NONE => first_answer f xs
      | SOME v => v ;

(* 8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option (notice the 2 arguments are curried) *)
fun all_answers f xs =
  foldl(fn (x, acc) => case f x of
                           SOME lst1 => (case acc of
                                             SOME lst2 => SOME(lst2 @ lst1)
                                           | NONE => NONE)
                         | NONE => NONE)
       (SOME []) xs;


(* 9a. Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard patterns it contains. *)
val count_wildcards  =
  g (fn () => 1) (fn _ => 0) ;


(* 9b. Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns it contains. *)
val count_wild_and_variable =
    g (fn () => 1 ) String.size ;

(* 9c. Use g to define a function count_some_var that takes a string and a pattern (as a pair) and returns the number of times the string appears as a variable in the pattern.*)
fun count_some_var (str, ptrn) =
  g (fn () => 0) (fn x => if x = str then 1 else 0) ptrn;

(* 10. Write a function check_pat that takes a pattern and returns true if and only if all the variables appearing in the pattern are distinct from each other (i.e., use different strings).  *)
fun check_pat p =
  let
      fun extract_var_names p = 
        case p of
            Variable s => [s]
          | TupleP ts => List.foldl (fn (x, l) => extract_var_names(x) @ l) [] ts
          | ConstructorP (_, cp) => extract_var_names(cp)
          | _ => []
                     
      fun all_unique lst =
        case lst of
            [] => true
          | x :: xs' => not(List.exists (fn v => v = x) xs') andalso all_unique(xs')    
  in
      all_unique(extract_var_names(p))
  end;

(* 11. Write a function match that takes a valu * pattern and returns a (string * valu) list option, namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does. *)
fun match valptrn =
  case valptrn
   of (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const v, ConstP v') => if v = v' then SOME [] else NONE
    | (Tuple vs, TupleP ps) =>
      if length(vs) = length(ps)
      then all_answers match (ListPair.zip(vs, ps))
      else NONE
    | (Constructor(s2, v), ConstructorP(s1, p)) => 
      if s1 = s2
      then match(v, p)
      else NONE
    | _ => NONE;


(* 12. Write a function first_match that takes a value and a list of patterns and returns a (string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where lst is the list of bindings for the first pattern in the list that matches. *)
fun first_match v patternlist =
  SOME(first_answer (fn p => match(v, p))
                    patternlist)
  handle NoAnswer => NONE;
                      
                      



