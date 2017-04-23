(* 1 *)
fun is_older (date_a : int * int * int, date_b : int * int * int) =
  if #1 date_a <> #1 date_b
  then #1 date_a < #1 date_b
  else if #2 date_a <> #2 date_b
  then #2 date_a < #2 date_b
  else if #3 date_a <> #3 date_b
  then #3 date_a < #3 date_b
  else false;

(* 2 *)
fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then
      1 + number_in_month(tl dates, month)
  else
      0 + number_in_month(tl dates, month);

(* 3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months);

(* 4 *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if 1 = number_in_month([hd dates], month)
  then hd(dates)::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month);

(* 5 *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);

(* 5 *)
fun get_nth (strs : string list, n : int) =
  if null strs
  then ""
  else if n = 1
  then hd strs
  else get_nth(tl strs, n - 1);

val english_months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

fun date_to_string (date : int * int * int) =
  get_nth(english_months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date);

(* 8 *)
fun number_before_reaching_sum (sum : int, l : int list) =
  if sum <= hd l
  then 0
  else
      1 + number_before_reaching_sum(sum - hd l, tl l);

val month_has_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 100];
(* 9 *)
fun what_month (day_of_year : int) =
  1 + number_before_reaching_sum(day_of_year, month_has_days);

(* 10 *)
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else
      what_month (day1) :: month_range (day1 + 1, day2);

(* 11 *)
fun oldest (dates : (int * int * int) list) =
  if null dates
  then NONE
  else if null (tl dates)
  then SOME (hd dates)
  else if is_older (hd dates, hd (tl dates))
  then oldest (hd(dates)::tl(tl dates))
  else oldest (tl dates);


fun filter_same_month (months : int list) =
  let
      fun has_same_elem (i : int, l : int list) =
        if null l
        then false
        else if i = hd l
        then true
        else has_same_elem (i, tl l)
  in
      if null months
      then []
      else if has_same_elem (hd months, tl months)
      then filter_same_month (tl months)
      else hd(months)::filter_same_month(tl months)
  end;

(* 12 *)
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
  number_in_months(dates, filter_same_month(months));

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
  dates_in_months(dates, filter_same_month(months));

(* 13 *)
fun reasonable_date (date : int * int * int) =
  if #1 date <= 0
  then false
  else if #2 date < 1 orelse #2 date > 12
  then false
  else if #3 date > 366 orelse #3 date <= 0
  then false
  else if #2 date = 2 andalso #3 date = 29
  then #1 date mod 400 = 0 orelse (#1 date mod 4 = 0 andalso #1 date mod 10 <> 0)
  else true;
