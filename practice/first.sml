val x = 34;

(* we are not "assigning" to y, we are simply evaluating the expression
 x "eagerly" and creating a binding between the resulting value and 
the identifier y *)
val y = x;


val x = 35;
(* y is still 34 *)
(* this binding "shadows" the previous biding *)
