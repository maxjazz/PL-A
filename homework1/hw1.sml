(***********************************************************************************
val is_older = fn : (int * int * int) * (int * int * int) -> bool
val number_in_month = fn : (int * int * int) list * int -> int
val number_in_months = fn : (int * int * int) list * int list -> int
val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list
val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list
val get_nth = fn : string list * int -> string
val date_to_string = fn : int * int * int -> string
val number_before_reaching_sum = fn : int * int list -> int
val what_month = fn : int -> int
val month_range = fn : int * int -> int list
val oldest = fn : (int * int * int) list -> (int * int * int) option
****************************************************************************************)

(* 1 *)
fun is_older ( date1: int*int*int, date2: int*int*int ) =
    (#1 date1 < #1 date2  )
    orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2 )
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2);

(* 2 *)
fun number_in_month (dl : (int*int*int) list, m : int) =
  if null dl
  then 0
  else
      let
          val nim = number_in_month(tl dl, m)
      in
          if #2 (hd dl) = m
          then 1+nim
          else nim
      end;

(* 3 *)
fun number_in_months (dl : (int*int*int) list, ml : int list) =
  if null ml
  then 0
  else
      number_in_month (dl, hd ml) + number_in_months (dl, tl ml);
(* 4 *)
fun dates_in_month (dl : (int*int*int) list, d : int) =
  if null dl
  then []
  else
      let
          val dim = dates_in_month (tl dl, d)
      in
          if #2 (hd dl) = d
          then (hd dl)::dim
          else dim
      end;

(* 5 *)
fun dates_in_months (dl : (int*int*int) list, ml:int list) =
  if null ml
  then []
  else
      dates_in_month (dl, hd ml)::dates_in_months (dl, tl ml);

(* 6 *)
fun get_nth (sl : string list, n : int) =
  if n = 1
  then hd sl
  else get_nth (tl sl, n-1);

(* 7 *)
fun date_to_string ( date: (int*int*int)) =
  let
      val months = ["January", "February", "March",
                    "April", "May", "June",
                    "July", "August", "September",
                    "November", "October", "December"]
  in
      get_nth (months, #2 date) ^
      " "  ^ Int.toString (#3 date) ^
      ", " ^ Int.toString (#1 date)
  end;

(* 8 *)
fun number_before_reaching_sum ( sum : int, dl : int list) =
  if hd dl >= sum
  then 0
  else 1 + number_before_reaching_sum (sum - hd dl, tl dl);

(* 9 *)
fun what_month (day : int) =
  let
      val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      1+number_before_reaching_sum (day, months)
  end;

(* 10 *)
fun month_range (d1: int, d2: int) =
  if d1>=d2
  then []
  else what_month d1 :: month_range(d1+1, d2);

(* 11 *)
fun oldest (dl : (int*int*int) list) =
  if null dl
  then NONE
  else
      let
          val rest_oldest = oldest(tl dl)
      in
          if isSome rest_oldest andalso is_older((valOf rest_oldest), (hd dl))
          then rest_oldest
          else SOME (hd dl)
      end;




          

