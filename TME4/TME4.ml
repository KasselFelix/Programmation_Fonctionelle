(*------------------- TME 4 --------------------*)
(*---------------- Ex 4 ---------------------*)

(*----------------------- Question 1 ------------------------*)
type 'a btree =
  |Empty
  |Node of 'a btree * 'a * 'a btree

let rec insert (x : 'a) (bt : 'a btree) : 'a btree =
  match bt with
  | Empty -> Node(Empty,x,Empty)
  | Node(bt1,a,bt2) -> if x<a then Node((insert x bt1),a,bt2) else Node(bt1,a,(insert x bt2))

let arbre = Node(Node(Empty,1,Empty),2,Node(Empty,4,Node(Empty,6,Empty)))
let arbre2 = insert 5 arbre

(*------------------- Question 2 ------------------------*)
    (*q2.1*)
let rec from_list (bt: 'a list): 'a btree =
    match bt with
    | [] -> Empty
    |h::t -> insert h (from_list t)

    (*q2.2*)
let from_list_term (bt : 'a list) : 'a btree =
  let rec loop (liste : 'a list) (accu : 'a btree) : 'a btree =
    match liste with
    |[]-> accu
    |h::t -> insert h (loop t accu)
  in loop bt Empty

    (*q2.3*)
let from_list_it (bt : 'a list) : 'a btree =
  List.fold_left (fun x y -> insert y x) Empty (List.rev bt)

(*--------------- Question 3 --------------------------*)

let rec to_list (bt: 'a btree) : 'a list=
  match bt with
  |Empty -> []
  |Node(bt1, x, bt2) -> (to_list bt1)@(x::to_list bt2)

(*------------------ Question 4 ---------------------*)
let tri (liste : 'a list) : 'a list =
  let x = from_list liste in to_list x

    (*----------------- Ex 5 ------------------------*)
