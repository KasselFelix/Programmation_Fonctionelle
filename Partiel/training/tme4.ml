(* Exercice IV *)
type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

let rec insert ( x: 'a ) ( bt: 'a btree ): 'a btree =
  match bt with
  | Empty -> Node( x, Empty, Empty )
  | Node( e, g, d ) ->
    if x < e then Node( e, insert x g, d )
    else          Node( e, g, insert x d )

let rec from_list ( l: 'a list ): 'a btree =
  match l with
  | [] -> Empty
  | h :: t -> insert h ( from_list t )

let ter_from_list ( l: 'a list ): 'a btree =
  let rec loop ( loop_list: 'a list ) ( bt: 'a btree ): 'a btree =
    match loop_list with
    | [] -> bt
    | h :: t -> loop t ( insert h bt )
  in loop l Empty

let fold_from_list ( l: 'a list ): 'a btree =
  List.fold_left ( fun bt x -> insert x bt ) Empty l

let rec to_list ( bt: 'a btree ): 'a list =
  match bt with
  | Empty -> []
  | Node( e, g, d ) -> ( to_list g ) @ [ e ] @ ( to_list d )

let tri ( l: 'a list ) : 'a list =
  l |> from_list |> to_list

(* Exercice V *)
type task = ( int * string )

let insert_task ( t: task ) ( bt: task btree ): task btree =
  let p1, _ = t in
  match bt with
  | Empty -> Node( t, Empty, Empty )
  | Node( e, g, d ) ->
    let p2, _ = e in
      if p1 < p2 then Node( e, insert t g, d )
      else            Node( e, g, insert t d )

let rec take_max ( bt: task btree ): string =
  match bt with
  | Empty -> raise ( Invalid_argument "Empty tree " )
  | Node( ( _, name ), _, Empty ) -> name
  | Node( _, _, d ) -> take_max d

let rec remove_max ( bt: task btree ): task btree =
  match bt with
  | Empty -> raise ( Invalid_argument "Empty tree " )
  | Node( e, g, Empty ) -> g
  | Node( e, g, d ) -> Node( e, g, remove_max d )

let rec take_and_remove_max ( bt: task btree ): ( string * task btree ) =
  match bt with
  | Empty -> raise ( Invalid_argument "Empty tree " )
  | Node( e, g, Empty ) -> let _, name = e in ( name, g )
  | Node( e, g, d ) -> let name, d' = take_and_remove_max d in ( name, Node( e, g, d' ) )

type action =
  | Pop
  | Push of ( int * string )

let rec exec ( l: action list ) ( bt: task btree ): string list =
  match l with
  | [] -> []
  | Pop    :: other_actions -> let name, new_bt = take_and_remove_max bt in name :: ( exec other_actions new_bt )
  | Push a :: other_actions -> exec other_actions ( insert_task a bt )
