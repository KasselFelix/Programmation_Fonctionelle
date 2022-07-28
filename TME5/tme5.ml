(*------------------- TD 5 --------------------*)

(*---------------- Exercice 1 ------------------*)

(*------------- Question 1 --------------------*)
type 'a gtree =
  |Empty
  |Node of ('a*('a gtree)list)

let rec size (gt:('a gtree)) : int =
  match gt with
  |  Empty->0
  |Node(_,gts)-> 1 + (size_forest gts)
and size_forest (gts:('a gtree)list) : int =
  match gts with
  |  []->0
  |h::t->(size h)+(size_forest t)

let rec size2(gt : 'a gtree) : int =
  match gt with
    Empty->0
  |Node(_,gts)->1 + List.fold_left(fun r gt ->r + (size2 gt)) 0 gts

(*------------------------- Question 2 ----------------*)

let rec height (gt : 'a gtree) : int =
  match gt with
    Empty-> 0
  |Node(_,gts)->1+(height_forest gts)
and height_forest (gts:('a gtree)list):int=
  match gts with
    []->0
  |h::t->(max (height h) (height_forest t))

let rec height2 (gt:'a gtree):int=
  match gt with
    Empty->0
  |Node(_,gts)->1+(List.fold_left(fun r gt ->max r (height2 gt)) 0 gts)

(*----------------- Question 3 ------------------------------*)

let rec to_list (gt:'a gtree) : 'a list =
  match gt with
    Empty->[]
  |Node(x,gts)-> x::(to_list_forest gts)
and to_list_forest(gts : ('a gtree)list) : 'a list =
  match gts with
    []->[]
  |h::t->(to_list h)@(to_list_forest t)

let rec to_list2(gt:'a gtree):'a list =
  match gt with
    Empty->[]
  |Node(x,gts)->x::(List.fold_left (fun l gt -> l@(to_list2 gt)) [] gts)

let rec to_list3(gt:'a gtree):'a list =
  match gt with
    Empty->[]
  |Node(x,gts)->x::(List.concat(List.map to_list3 gts))

(*----------- Exercice 2 -------------*)

(*------------- Question 1 ----------------*)

let empty_forest(gts:('a gtree)list):bool=
  match gts with
    []->true
  |_->(List.for_all (fun gt -> (gt=Empty)) gts)

let rec empty_forest2(gts:('a gtree)list):bool=
  match gts with
    []->true
  |h::t->(h==Empty)&&(empty_forest2 t)

let rec empty_forest3(gts :('a gtree)list):bool=
  match gts with
    []->true
  |Empty::t->(empty_forest3 t)
  |_->false

(*-------------- Question 2 --------------*)

let rec nb_leaves(gt:'a gtree):int =
  match gt with
    Empty -> 0
  |Node(_,gts)->if(empty_forest gts) then 1
    else (nb_leaves_forest gts)
and nb_leaves_forest (gts:('a gtree)list):int =
  match gts with
    []->0
  |h::t->(nb_leaves h) + (nb_leaves_forest t)

(*----------- Question 3 ------------------*)

let rec leaves(gt:'a gtree):'a list=
  match gt with
    Empty->[]
  |Node(x,gts)->if(empty_forest gts) then [x]
    else (leaves_forest gts)
and leaves_forest (gts:('a gtree)list):'a list =
  match gts with
    []->[]
  |h::t->(leaves h)@(leaves_forest t)

(*-------- Exercice 3 -----------------*)

(*------------- Question 1 ----------*)

let rec remove_Empty(gt:'a gtree):'a gtree =
  match gt with
    Empty-> raise (Invalid_argument "remove_Empty")
  |Node(x,gts)->Node(x,remove_Empty_forest gts)
and remove_Empty_forest(gts:('a gtree)list):('a gtree)list=
  match gts with
    []->[]
  |Empty::t->(remove_Empty_forest t)
  |h::t->(remove_Empty h)::(remove_Empty_forest t)

(*----------------- Question 2 ------------------*)

let rec not_exists_Empty(gt:'a gtree):bool=
  match gt with
    Empty->false
  |Node(x,gts)->(not_exists_Empty_forest gts)
and not_exists_Empty_forest (gts:('a gtree)list):bool=
  match gts with
    []->false
  |h::t->(not_exists_Empty h)&&(not_exists_Empty_forest t)

(*---------- Exercice 3 ---------------*)
(*----------------- Question 1 ---------------*)
