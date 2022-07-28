(* Q1 *)
let rec merge inf l1 l2 =
  match ( l1, l2 ) with
  | ( h1 :: t1, h2 :: t2 ) ->
    if ( inf h1 h2 )
    then h1 :: ( merge inf t1 l2 )
    else h2 :: ( merge inf l1 t2 )
  | _ -> l1 @ l2
(* Dernier cas: au moins une des listes est vide. *)

(* Q2 *)
let ter_merge inf l1 l2 =
  let rec merge_rev l1 l2 acc =
    match ( l1, l2 ) with
    | ( h1 :: t1, h2 :: t2 ) ->
      if ( inf h1 h2 )
      then merge_rev t1 l2 ( h1 :: acc )
      else merge_rev l1 t2 ( h2 :: acc )
    | _ -> l1 @ l2 @ acc
  in List.rev ( merge_rev l1 l2 [] )

(* Q3 *)
let rec split l =
  match l with
  |      [] -> ( [], [] )
  | h :: [] -> ( [ h ], [] )
  | h1 :: h2 :: t ->
    let ( t1, t2 ) = split t in
    ( h1 :: t1, h2 :: t2 )

(* Q4 *)
let rec ter_split l =
  let rec split_rev l t1 t2 =
    match l with
    |      [] -> ( t1, t2 )
    | h :: [] -> ( h :: t1, t2 )
    | h1 :: h2 :: t -> split_rev t ( h1 :: t1 ) ( h2 :: t2 )
  in let ( l1_rev, l2_rev ) = ( split_rev l [] [] )
  in ( List.rev l1_rev, List.rev l2_rev )

(* Q5 *)
let rec merge_sort inf l =
  let ( l1, l2 ) = split l
  in match ( l1, l2 ) with
  |       [],       [] -> []
  | h1 :: [],       l2 -> merge inf                  l1   ( merge_sort inf l2 )
  |       l1, h2 :: [] -> merge inf ( merge_sort inf l1 )                  l2
  |       l1,       l2 -> merge inf ( merge_sort inf l1 ) ( merge_sort inf l2 )

(* Q6 *)
let rec padding l1 l2 x =
  match ( l1, l2 ) with
  | [], [] -> ( [], [] )
  | h1 :: t1, [] ->
    let ( t1, padded_t2 ) = padding t1 [] x
    in ( t1, x :: padded_t2 )
  | [], h2 :: t2 ->
    let ( padded_t1, t2 ) = padding [] t2 x
    in ( x :: padded_t1, t2 )
  | h1 :: t1, h2 :: t2 ->
    let ( padded_t1, padded_t2 ) = padding t1 t2 x
    in ( h1 :: padded_t1, h2 :: padded_t2 )

(* Q7 *)
let rec lex inf min l1 l2 =
  let ( l1, l2 ) = padding l1 l2 min
  in match ( l1, l2 ) with
  | [], _ -> true
  | _, [] -> false
  | h1 :: t1, h2 :: t2 ->
    if h1 = h2 then lex inf min t1 t2
    else inf h1 h2

(* Q8, fonction cmp_bool provenant du PDF. *)
let cmp_bool (b1:bool) (b2:bool) : bool =
match b1, b2 with
false, true -> true
  | _ -> false

let sort_bool_list l = merge_sort ( lex cmp_bool false ) l
