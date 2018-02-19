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
fun number_in_month (dates: (int*int*int) list, date: int) =
    if null dates
    then 0
    else let val partial_ans = number_in_month(tl dates, date)
	     val first = hd dates
	 in
	     if (#2 first) = date
	     then 1 + partial_ans
	     else partial_ans
	 end
