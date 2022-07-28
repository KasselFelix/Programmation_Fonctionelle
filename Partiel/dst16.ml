let desserts:((string*string list)list) =
  [
    ( "gateau chocolat" ,
      [ "chocolat"; "oeuf"; "farine"; "sucre"; "beurre" ] );
    ( "gateau yaourt" ,
      [ "yaourt"; "oeuf"; "farine"; "sucre" ] );
    ( "crepes" ,
      [ "oeuf"; "farine"; "lait" ] );
    ( "quatre-quarts" ,
      [ "oeuf"; "farine"; "beurre"; "sucre" ] );
    ( "kouign amann" ,
      [ "farine"; "beurre"; "sucre" ] )
  ]

let rec ingredient (rs:(string * string list) list) (rname:string) :string list=
  match rs with
  |(r,i)::t-> if r=rname then i
    else (ingredient t rname)
  |[]->[]

let rec nb_ingredients (rs:(string * string list) list) (rname:string) :int=
  match rs with
  |((r,i)::t)-> if r=rname then List.length(i)
    else (nb_ingredients t rname)
  |[]->0

let nb_ingredients2 rs rname=
  List.length(ingredient rs rname)

let rec repeat (x:'a)(n:int):'a list=
  if (n=0) then []
  else x::(repeat x (n-1) )

let rec flatten  (l:('a list)list) : 'a list =
  match l with
  |h::t-> h@ (flatten t)
  |[]->[]

type film_t=(string*(string*string)*int*(string*string)list)list
let films:((string*(string*string)*int*(string*string)list)list) =
  [
    ("Pulp Fiction", ("Tarantino","Quentin"), 1994, [("Travolta","John") ; ("Thurman","Uma")]);
    ("Psychose" , ("Hitchcock","Alfred") , 1960, [("Perkins","Anthony"); ("Leigh","Janet")]);
    ("Shining" , ("Kubrick","Stanley") , 1980, [("Nicholson","Jack") ; ("Duvall","Shelley")]);
    ("Nassim moi j'ai valider" , ("Kubrick","Stanley") , 1980, [("Nicholson","Jack") ; ("Duvall","Shelley")]);
    ("Barry Lyndon", ("Kubrick","Stanley") , 1975, [("Dullea","Keir") ; ("Lockwood","Gary")]);
    ("Grease" , ("Randal","Kleiser") , 1978, [("Travolta","John") ; ("Olivia","Newton-John")]);
  ]

let titres (l:film_t):string list=
  List.map (fun (titre,(_,_),_,_)->titre) l

let film_1980 (l:film_t):string list=
  let r = List.filter(fun (_,(_,_),date,_)-> date = 1980) l in titres r

(*let film_by_actor (prenom:string)(nom:string) (l:film_t):string list=
  let r = List.filter (fun (_,(_,_),_,x)->
        let rec loop x =
        match x with
        |[]->false
        |h::t-> if h=(prenom,nom) then true else loop t in x ) l in titres r*)

  let film_by_actor2 (prenom:string)(nom:string) (l:film_t):string list=
    titres ( List.filter (fun (_,(_,_),_,x)-> List.exists (fun (p,n)-> (prenom,nom)=(p,n) )x ) l )



let desserts :(string*(string list))list =
  [
    ( "gateau chocolat" ,
      [ "chocolat"; "oeuf"; "farine"; "sucre"; "beurre" ] );
    ( "gateau yaourt" ,
      [ "yaourt"; "oeuf"; "farine"; "sucre" ] );
    ( "crepes" ,
      [ "oeuf"; "farine"; "lait" ] );
    ( "quatre-quarts" ,
      [ "oeuf"; "farine"; "beurre"; "sucre" ] );
    ( "kouign amann" ,
      [ "farine"; "beurre"; "sucre" ] )
  ]
let ingredient (s:(string*(string list))list)(rs:string):string list=
  let r=List.filter(fun (x,y) -> x=rs) s in
  match r with
  |[(x,y)] -> y
  |_->[]
