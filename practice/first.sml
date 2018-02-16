val x = 34;

(* we are not "assigning" to y, we are simply evaluating x "eagerly" and 
creating a binding between that value and y *)
val y = x;


val x = 35;
(* y is still 34 *)
