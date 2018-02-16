(* max function of a list using options *)
(* int list -> int option *)
fun max (l: int list) =
    if null l
    then NONE
    else let
	fun max_helper (l: int list) =
	    if null (tl l)
	    then hd l
	    else let val tl_ans = max_helper(tl l)
		 in
		     if hd l > tl_ans
		     then hd l
		     else tl_ans
		 end
    in
	SOME (max_helper l)
    end
	
