(* Exercice V *)

let float_of_3int ( a, b, c ) = (
    float_of_int a,
    float_of_int b,
    float_of_int c
)

let valeur_poly abc x =
    let ( a, b, c ) = float_of_3int abc in
    a *. x *. x +. b *. x +. c

let discriminant ( a, b, c ) =
    b * b - 4 * a * c

let nb_solution delta =
    if delta < 0 then 0 else
    if delta = 0 then 1 else
                      2

let solutions a b c =
  let delta = discriminant ( a, b, c ) in
  let sqrt_delta = delta |> float_of_int |> sqrt in
  let ( a', b', c' ) = float_of_3int ( a, b, c ) in
    match nb_solution delta with
    | 0 -> ( 0.0, 0.0 )
    | 1 -> ( -. a' /. ( 2.0 *. a' ), -. b' /. ( 2.0 *. a' ) )
    | _ -> (
        ( -. b' -. sqrt_delta ) /. ( 2.0 *. a' ),
        ( -. b' +. sqrt_delta ) /. ( 2.0 *. a' )
      )

(* Le type de solutions est int -> int -> int -> float * float *)

(* Exercice VI *)
let rec p n =
    if n <= 0 then raise ( Invalid_argument "Il faut que n >= 1" ) else
    if n = 1 then 1.0
    else 1.0 /. float_of_int ( n * n ) +. p ( n - 1 )

(* sqrt ( 6.0 *. p   500 ) = 3.13968412313872181 *)
(* sqrt ( 6.0 *. p  5000 ) = 3.14140168095094285 *)
(* sqrt ( 6.0 *. p 50000 ) = 3.14157355512957048 *)
(* p = 500000 mène à un stack overflow à cause de la pile des fonctions récursives *)

let rec ter_p n =
    if n <= 0 then raise ( Invalid_argument "Il faut que n >= 1" ) else
      let rec loop acc i =
        if ( i = 1 )
        then acc
        else loop ( acc +. 1.0 /. float_of_int ( i * i ) ) ( i - 1 )
      in loop 1.0 n
(* sqrt ( 6.0 *. ter_p 500000 ) = 3.141590743731832 *)
(* L'appel de la fonction récursive étant en position terminale, chaque appel est dépilé à la dernière instruction de la fonction.
   Ainsi, la pile de fonctions ne croît pas excessivement. *)

(* Exercice VII *)
let f a x = ( x +. a /. x ) /. 2.0
(* Le type de f est float -> float -> float *)

let rec sqrt_n n a x0 =
  if n = 0 then x0
  else f a ( sqrt_n ( n - 1 ) a x0 )

(* Nous observons que sqrt_n tend vers la racine carrée pour de grandes valeurs de n *)

let eq_eps e x y = abs_float( x -. y ) < e

let rec sqrt_x e a x0 =
    let xn = f a x0 in
    if eq_eps e xn x0 then xn
    else sqrt_x e a xn

(* Des valeurs de e décroissantes entraînent une augmentation de la précision de la racine carrée *)
