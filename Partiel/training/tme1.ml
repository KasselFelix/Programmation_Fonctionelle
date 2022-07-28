
(* Q1 *)
let xor x y = x || y && not ( x && y )

(* Q2 *)
(*
    x < y se traduit par - x . y
    x = y se traduit par x = y
    x > y se traduit par - y . x
*)
type cmp = bool * bool * bool

(* Q3 *)
(* cmp1 peut renvoyer 3 valeurs : *)
let a_inf_b = true, false, false (* cas a = false et b = true *)
let a_eq_b = false, true, false (* cas a = b *)
let a_sup_b = false, false, true (* cas a = true et b = false *)

(* Q4 *)
let cmp1 a b = ( not a && b, a = b, not b && a )

let () = assert( cmp1 false false = a_eq_b )
let () = assert( cmp1 false true = a_inf_b )
let () = assert( cmp1 true false = a_sup_b )
let () = assert( cmp1 true true = a_eq_b )

(* Q5 *)
let lex ( lt1, eq1, gt1 ) ( lt2, eq2, gt2 ) =
  lt1 || eq1 && lt2,
  eq1 && eq2,
  gt1 || eq1 && gt2

(* Q6 *)
type duet = bool * bool

let cmp2 ( a1, a2 ) ( b1, b2 ) =
  lex ( cmp1 a1 b1 ) ( cmp1 a2 b2 )

(* Q7 *)
type quartet = bool * bool * bool * bool

let cmp4 ( a1, a2, a3, a4 ) ( b1, b2, b3, b4 ) =
  lex ( cmp2 ( a1, a2 ) ( b1, b2 ) ) ( cmp2 ( a3, a4 ) ( b3, b4 ) )

(* Q8 *)
type octet = bool * bool * bool * bool * bool * bool * bool * bool

let cmp8 ( a1, a2, a3, a4, a5, a6, a7, a8 ) ( b1, b2, b3, b4, b5, b6, b7, b8 ) =
  lex ( cmp4 ( a1, a2, a3, a4 ) ( b1, b2, b3, b4 ) ) ( cmp4 ( a5, a6, a7, a8 ) ( b5, b6, b7, b8 ) )

(* Q9 *)
(* Les 3 fonctions ci-dessous sont à utiliser pour la fin du tme. Vous
   compléterez les annotations de types des fonctions suivantes en utilisant les
   types que vous aurez définis pendant le tme *)

let i2b (n: int): bool =
  n <> 0

let i2q (n: int): quartet =
  let b0 = i2b (n mod 2) in
  let b1 = i2b (n/2 mod 2) in
  let b2 = i2b (n/4 mod 2) in
  let b3 = i2b (n/8 mod 2) in
    (b3,b2,b1,b0)

let i2o (n: int): octet =
  let b0 = i2b (n mod 2) in
  let b1 = i2b (n/2 mod 2) in
  let b2 = i2b (n/4 mod 2) in
  let b3 = i2b (n/8 mod 2) in
  let b4 = i2b (n/16 mod 2) in
  let b5 = i2b (n/32 mod 2) in
  let b6 = i2b (n/64 mod 2) in
  let b7 = i2b (n/128 mod 2) in
  (b7,b6,b5,b4,b3,b2,b1,b0)

(* Q10 *)
let () = assert( i2b 1 && not ( i2b 0 ) )

let int_to_bool b = if b then 1 else 0

let rec test_quartet n =
  ( let ( b3, b2, b1, b0 ) = i2q n in

  )
  && ( n = 0 || test_quartet ( n - 1 ) )
(* Q11 *)
