(*
1. This problem involves using first-name substitutions to come up with alternate names.
 *)


fun same_string (s1 : string, s2 : string) =
  s1 = s2;

(* 1A. Takes a string and a string list. Return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. *)
fun all_except_option (str, str_lst) =
  case str_lst of
      [] => NONE
    | head::tail => if same_string(head, str)
                    then SOME tail
                    else case all_except_option(str, tail) of
                             NONE => NONE
                           | SOME xs => SOME (head::xs);

(* 1B. Takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list *)
fun get_substitutions1 (str_list, str) =
  case str_list of [] => []
                 | x::xs => case all_except_option (str, x) of
                                NONE => get_substitutions1(xs, str)
                              | SOME st => st @ get_substitutions1(xs, str);

(* 1C. uses a tail-recursive local helper function *)
fun get_substitutions2 (str_list, str) =
  let fun aux (list, acc) =
        case list of [] => acc
                   | x::xs => case all_except_option (str, x) of
                                  NONE => aux (xs, acc)
                                | SOME value => aux (xs, value @ acc)
  in
      aux(str_list, [])
  end;


(* 1D.  similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list). T) *)

fun similar_names (list, name) =
  let val {first = f, middle = m, last = l} = name
      fun generate_names  (list) =
        case list of [] => []
                   | x::xs => {first = x, middle = m, last = l} :: generate_names(xs)
  in
      generate_names(get_substitutions2(list, f))
  end;


(*
2. This problem involves a solitaire card game invented just for this question. You will write a program that tracks the progress of a game; writing a game player is a challenge problem. You can do parts (a)–(e) before understanding the game if you wish.
*)

datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int;
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw; 

exception IllegalMove;

(* 2A. Write a function card_color, which takes a card and returns its color (spades and clubs are black, diamonds and hearts are red). *)
fun card_color (card) =
  case card of (Spades, _) => Black
           | (Clubs, _) => Black
           | (_,_) => Red;

(* 2B. Write a function card_value, which takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10) *)
fun card_value (card) =
case card of (_, Num n) => n
           | (_, Ace ) => 11
           | (_, _) =>10;


(* 2C. Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. *)
fun remove_card (cs , c , e) =
  case cs of [] => raise e
           | head::tail  => if head = c
                            then tail
                            else head::remove_card(tail, c, e);

(* 2D. Write a function all_same_color, which takes a list of cards and returns true if all the cards in the list are the same color. *)
fun all_same_color (cs) =
  case cs of [] => true
           | x::[] =>true
           | x::y::xs => card_color(x) = card_color(y) andalso all_same_color(y::xs);

(* 2E. Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally defined helper function that is tail recursive *)
fun sum_cards (cs) =
  let fun sum(cs, acc) =
      case cs of [] => acc
               | x::xs => sum(xs, acc+card_value(x))
  in
      sum (cs,0)
  end;

(* 2F. Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes the score as described above. *)

fun score (cs, goal) =
  let fun sum_pre_score (cs, goal) =
        if sum_cards(cs) < goal
        then (goal - sum_cards(cs))
        else 3*(sum_cards(cs)-goal)
  in
      if all_same_color(cs)
      then sum_pre_score (cs, goal) div 2
      else sum_pre_score (cs, goal)
  end
;

  (* 2G. Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list (what the player “does” at each point), and an int (the goal) and returns the score at the end of the game after processing (some or all of) the moves in the move list in order. *)
  fun officiate (cards, moves, goal) =
    let fun loop (current, cards_left, moves_left) =
          case moves_left of
              [] => score (current, goal)
            | (Discard c)::tail => loop ( remove_card(current, c, IllegalMove) , cards_left, tail)
            | Draw::tail => case cards_left of
                                [] => score (current, goal)
                              | c::rest => if sum_cards(c::current) > goal
                                           then score (c::current, goal)
                                           else
                                               loop (c::current, rest, tail)
    in
        loop ([],cards, moves)
    end;


