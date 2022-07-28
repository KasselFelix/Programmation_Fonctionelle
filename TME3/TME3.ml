(*Exercice 7 du TME3------------------------------------------------------------------------------------------*)

(*--------------------Question 1-------------------------------*)

let rec merge (comp:'a->'a->bool) (l1:'a list) (l2:'a list) : ('a list) =
  match l1,l2 with
    [],_->l2
  | _,[]->l1
  |h1::t1,h2::t2-> if (comp h1 h2) then h1::(merge comp t1 l2)
    else h2::(merge comp l1 t2)

(*--------------------Question 2-------------------------------*)

let merge_term (comp:'a->'a->bool) (l1:'a list) (l2:'a list) : ('a list) =
  let rec aux (l1:'a list) (l2:'a list) (accu: 'a list): ('a list) =
    match l1,l2 with
      [],[]-> List.rev accu
    | [], h2::t2-> (aux l1 t2 (h2::accu))
    | h1::t1,[]-> (aux t1 l2 (h1::accu))
    |h1::t1,h2::t2->if  (comp h1 h2) then (aux t1 l2 (h1::accu))
      else (aux l1 t2 (h2::accu))
  in (aux l1 l2 [])

(*--------------------Question 3-------------------------------*)

let rec split (l:'a list) : ('a list * 'a list)=
  match l with
    []-> ([],[])
  | h::[]-> ([h],[])
  | h1::h2::t->let (l1,l2)=split t in (h1::l1,h2::l2)

(*--------------------Question 4-------------------------------*)

let split_terminal (l: 'a list) : ('a list * 'a list)=
  let rec loop (l: 'a list) (accu:'a list * 'a list): ('a list * 'a list)=
    match l with
      []->accu
    | h::[]-> ([h],[])
    | h1::h2::t->let (l1,l2)=loop t accu in (h1::l1,h2::l2)
  in loop l ([],[])

(*--------------------Question 5-------------------------------*)

let rec merge_sort (f:'a->'a->bool) (liste: 'a list) : 'a list =
  match liste with
    []->[]
  |x::[]->[x]
  |_-> let (a,b)=(split liste) in (merge f (merge_sort f a) (merge_sort f b))

(*--------------------Question 6-------------------------------*)

let padding (l1:'a list) (l2:'a list) (k:'a) : ('a list*'a list) =
  let rec aux (l1: 'a list) (l2:'a list) (k:'a) (a:'a list) (b: 'a list):('a list*'a list)=
    match l1,l2 with
      x::xs,y::ys->aux xs ys k (x::a) (y::b)
    | x::xs,[]->aux xs l2 k (x::a) (k::b)
    | [],y::ys->aux (l1) ys k (k::a) (y::b)
    | [],[]->(List.rev a,List.rev b)
  in aux l1 l2 k [] []

(*--------------------Question 7-------------------------------*)

(*--------------------Question 8-------------------------------*)
