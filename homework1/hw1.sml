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

fun is_older ( date1: int*int*int, date2: int*int*int ) =
    (#1 date1 < #1 date2  )
    orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2 )
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2);

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

fun number_in_months (dl : (int*int*int) list, ml : int list) =
  if null ml
  then 0
  else
      number_in_month (dl, hd ml) + number_in_months (dl, tl ml);

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

fun dates_in_months (dl : (int*int*int) list, ml:int list) =
  if null ml
  then []
  else
      dates_in_month (dl, hd ml)::dates_in_months (dl, tl ml);



         
                   
