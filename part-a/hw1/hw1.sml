fun getYear(d : int * int * int) =
  #1 d

fun getMonth(d : int * int * int) = 
  #2 d

fun getDay(d : int * int * int) =
  #3 d


(* ----------------------------------------------------------- *)


(* #1 *)
fun is_older(d1 : (int * int * int), d2 : (int * int * int)) =
  if getYear(d1) < getYear(d2) then true
  else if getYear(d1) > getYear(d2) then false
  else if getMonth(d1) < getMonth(d2) then true
  else if getMonth(d1) > getMonth(d2) then false
  else if getDay(d1) < getDay(d2) then true
  else false

(* #2 *)
fun number_in_month(ds : (int * int * int) list, month : int) =
  if null ds
  then 0
  else
    let
      val m = getMonth(hd ds)
    in
      if m = month
      then 1 + number_in_month(tl ds, month)
      else number_in_month(tl ds, month)
    end

(* #3 *)
fun number_in_months(ds : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(ds, hd months) + number_in_months(ds, tl months)

(* #4 *)
fun dates_in_month(ds : (int * int * int) list, month : int) =
  if null ds
  then []
  else
    if getMonth(hd ds) = month
    then [hd ds] @ dates_in_month(tl ds, month)
    else dates_in_month(tl ds, month)

(* #5 *)
fun dates_in_months(ds : (int * int * int) list, months : int list) =
  if null months
  then []
  else
    dates_in_month(ds, hd months) @ dates_in_months(ds, tl months)

(* #6 *)
fun get_nth(xs : string list, n : int) =
  if n = 1
  then hd xs
  else get_nth(tl xs, n - 1)

(* #7 *)
fun date_to_string(date : (int * int * int)) =
  let
    val months = [
        "January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "December"
    ]
  in
    get_nth(months, getMonth(date)) ^ " " ^ Int.toString(getDay(date)) ^ ", " ^ Int.toString(getYear(date))
  end

(* #8 *)
fun number_before_reaching_sum(sum : int, xs : int list) =
  let
    fun helper(remaining : int list, total : int, idx: int) =
      if total >= sum
      then idx - 1
      else helper(tl remaining, hd remaining + total, idx + 1)
  in
    helper(xs, 0, 0)
  end

(* #9 *)
fun what_month(doy : int) =
  let
    val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(doy, months) + 1
  end

(* #10 *)
fun month_range(d1 : int, d2 : int) =
  if d1 > d2
  then []
  else [what_month(d1)] @ month_range(d1 + 1, d2)

(* #11 *)
fun oldest(ds : (int * int * int) list) =
  if null ds
  then NONE
  else
    let val tl_ans = oldest(tl ds)
    in
      if isSome tl_ans andalso is_older(valOf tl_ans, hd ds)
      then tl_ans
      else SOME(hd ds)
    end
