(*Exercice 5 du TME2--------------------------------------------------------------------------------------------*)

type iii=int*int*int
type fff=float*float*float

   (*-----------------------Question 1---------------------------------*)

let float_of_3int (x:iii):(fff)=
  let (a,b,c)=x in
  (float_of_int a,float_of_int b, float_of_int c)

    (*----------------------Question 2--------------------------------*)

let valeur_poly (f:iii) (x:float) : float =
  let (a,b,c) = (float_of_3int f) in
  a*.x*.x+.b*.x+.c

    (*---------------------Question 3---------------------------------*)

let discriminant (x:iii) : int =
  let (a,b,c)= x in
  b*b-4*a*c

        (*----------------Question 4---------------------------------*)

let nb_solution (d:int) : int =
  if d=0 then 1
  else if d>0 then 2
  else 0

      (*-------------------Question 5--------------------------------*)

let solution (x:iii) : float*float=
  let y = (discriminant x) in
  if (nb_solution y) = 0 then (0.,0.)
  else if (nb_solution y)=1 then
    let (a,b,c)=(float_of_3int x) in (((-.b)/.2.*.a),((-.b)/.2.*.a))
  else
    let (a,b,c)=(float_of_3int x) in (((-.b+.(sqrt (float_of_int y)))/.2.*.a),((-.b-.(sqrt (float_of_int y)))/.2.*.a))

(*Exercice 6 du TME2------------------------------------------------------------------------------------------*)

      (*--------------------Question 1-------------------------------*)

let rec serie (n:int) : float =
  let x= float_of_int n in
  if x < 2. then 1.
  else ((1./.(x *. x))+.(serie (n-1)))

(*Pour n= 500, on a:
  sqrt (6.*.(serie 500));;
  - : float = 3.13968412313872181

  Pour n= 5 000, on a :
  sqrt (6.*.(serie (5000)));;
  - : float = 3.14140168095094285

  Pour n = 50 000, on a :
  sqrt (6.*.(serie 50000));;
  - : float = 3.14157355512957048

  Pour n = 500 000, on a :
  sqrt (6.*. (serie 500000));;
  Stack overflow during evaluation (looping recursion?).


*)
      (*------------------------Question 2----------------------------*)
let p (n:int) : float =
    let rec loop (i:int) (accu:float) : float =
      let x= float_of_int i in
      if x>1. then loop (i-1) (1./.(x *. x)+. accu)
      else accu
    in loop n 1.

(*Pour n = 500 000, on a :
  sqrt (6.*. (p 500000 ));;
  - : float = 3.141590743731832

  cf cours car ici c'est calculé au fur et à mesure tandis que dans l'autre fonction tout est empilé et stocké et le calcul à lieu qu'à la fin.

*)

(*Exercice 7 du TME2----------------------------------------------------------------------------------------*)
      (*--------------------------Question 1------------------------*)
let f (a:float) (x:float): float =
  (1./.2.)*.(x+.a/.x) (*énoncé non comprise->demande plus d'explication*)

    (*--------------------------Question 2---------------------------*)

let rec sqrt_n (n:int) (a:float) (x0:float) : float =
  if n<2 then f a x0
  else sqrt_n (n-1) a x0 (*énoncé non comprise->demande plus d'explication*)

      (*------------------------Question 3----------------------------*)

let eq_eps (e:float) (x:float) (y:float):bool=
  if (abs_float (x-.y))<e then true
  else false

(*------------------------Question 4------------------------------*)

(*Question non réussi*)



(*----------------------------------------------------------------------------------------------------------------------------------*)
