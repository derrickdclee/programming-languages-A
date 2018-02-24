(* Qn 1 *)
fun is_older (date1: int * int * int, date2: int * int * int) =
    let
	fun compare_day (date1: int * int * int, date2: int * int * int) =
	    if (#3 date1) < (#3 date2)
	    then true
	    else false

	fun compare_month (date1: int * int * int, date2: int * int * int) =
	    if (#2 date1) < (#2 date2)
	    then true
	    else if (#2 date1) > (#2 date2)
	    then false
	    else compare_day (date1, date2)
			     
	fun compare_year (date1: int * int * int, date2: int * int * int) =
	    if (#1 date1) < (#1 date2)
	    then true
	    else if (#1 date1) > (#1 date2)
	    then false
	    else compare_month (date1, date2)
    in
	compare_year(date1, date2)
    end

(* Qn 2 *)
fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else let val partial_ans = number_in_month(tl dates, month)
	     val first = hd dates
	 in
	     if (#2 first) = month
	     then 1 + partial_ans
	     else partial_ans
	 end

(* Qn 3 *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else let val partial_ans = number_in_months(dates, tl months)
	     val how_many_in_first_month = number_in_month(dates, hd months)
	 in
	     partial_ans + how_many_in_first_month
	 end
	     
(* Qn 4 *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else let val partial_ans = dates_in_month(tl dates, month)
	     val first_date = hd dates
	 in
	     if (#2 first_date) = month
	     then first_date :: partial_ans
	     else partial_ans
	 end

(* Qn 5 *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else let val partial_ans = dates_in_months(dates, tl months)
	     val first_ans = dates_in_month(dates, hd months)
	 in
	     first_ans @ partial_ans
	 end

(* Qn 6 *)
fun get_nth (strs: string list, n: int) =
    if n = 1
    then hd strs
    else get_nth(tl strs, n - 1)

(* Qn 7 *)
fun date_to_string (date: (int*int*int)) =
    let
	val months_list = ["January", "February", "March", "April", "May", "June", "July",
			   "August", "September", "October", "November", "December"]
	val month_name = get_nth(months_list, #2 date)
    in
	month_name ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* Qn 8 *)
fun number_before_reaching_sum (sum: int, nums: int list) =
    let
	fun helper(rem: int, nums: int list, n: int) =
	    if rem - (hd nums) <= 0
	    then n
	    else helper(rem - hd nums, tl nums, n + 1)
    in
	helper(sum, nums, 0)
    end

(* Qn 9 *)
fun what_month (day_of_year: int) =
    let
	val num_days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day_of_year, num_days_in_months) + 1
    end

(* Qn 10 *)
fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* Qn 11 *)
fun oldest (dates: (int*int*int) list) =
    if null dates
    then NONE
    else let val partial_ans = oldest (tl dates)
	     val first_date = hd dates
	 in
	     if isSome partial_ans andalso is_older(valOf partial_ans, first_date)
	     then partial_ans
	     else SOME (first_date)
	 end

