(************ LEFTIST ************)
(********* Emilia Dębicka ********)
(* sprawdzający: Szymon Pobłocki *)

(* typ kolejki pirytetowej, skłajadący się z liścia lub kolejno z lewego poddrzewa, wartości w węźle,
 * prawego poddrzewa i maksymalnej prawej wysokości w danym węźle *)
type 'a queue = Leaf | Node of 'a queue * 'a * 'a queue * int

(* wyjątek jeśli kolejka jest pusta *)
exception Empty

(* deklaracja pustej kolejki *)
let empty = Leaf;;

(* zwraca true dla pustej kolejki *)
let is_empty q =
    q = Leaf;;

(* funkcja pomocnicza zwracająca wysokość drzewa *)
let height q =
    match q with
    |Leaf -> 0
    |Node(_, _, _, h) -> h;;

(* funkcja łącząca dwie kolejki *)
let rec join d1 d2 =
    match (d1, d2) with
    |(d, Leaf) | (Leaf, d) -> d
    |Node(l1, x1, r1, h1), Node(l2, x2, r2, h2) ->
        if x2 < x1 then join d2 d1 else (* d1 zawsze będzie miało mniejszą wartość w węźle *)
            let d3 = join r1 d2
            in let h3 = height d3
            in let hl1 = height l1
            in if h3 < hl1 then
                Node(l1, x1, d3, (h3+1))
            else
                Node(d3, x1, l1, (hl1+1));;

(* [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *)
let add e q =
    join (Node(Leaf, e, Leaf, 1)) q;;

(* dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
 * jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
 * Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q =
    match q with
    |Leaf -> raise Empty
    |Node(l, x, r, h) -> (x, (join l r));;




(*PRZYKŁADY*)
(*
let a = Node(Node(Node(Node(Leaf, 19, Leaf, 1), 8, Leaf, 1), 2, Node(Node(Leaf, 15, Leaf, 1), 9, Node(Leaf, 20, Leaf, 1), 2), 3), 1, Node(Leaf, 4, Leaf, 1), 2);;

let b = Node(Node(Node(Node(Leaf, 12, Leaf, 1), 10, Leaf, 1), 6, Leaf, 2), 2, Node(Node(Node(Leaf, 15, Leaf, 1), 12, Node(Leaf, 16, Leaf, 1), 2), 8, Leaf, 2), 3);;
assert (fst (delete_min b) = 2);;

let to_tree lista =
    let rec pom lst acc =
        match lst with
        |[] -> acc
        |h::t -> pom t (add h acc)
    in pom lista Leaf;;

let c = to_tree [1; 4; 2; 20; 15; 19; 9; 8];;
let d = to_tree [3; 5; 10; 2; 2; 7; 22];;

let e = Node(Node(Node(Node(Node(Leaf, 7, Leaf, 1), 6, Leaf, 1), 5, Leaf, 1), 4, Leaf, 1), 1, Node(Leaf, 3, Leaf, 1), 2);;
let f = Node(Node(Node(Node(Node(Leaf, 19, Leaf, 1), 11, Node(Leaf, 12, Leaf, 1), 2), 8, Leaf, 1), 3, Node(Node(Leaf, 18, Leaf, 1), 9, Leaf, 1), 2), 2, Leaf, 1);;
assert (join e f =
Node
 (Node
   (Node
     (Node (Node (Node (Leaf, 19, Leaf, 1), 11, Node (Leaf, 12, Leaf, 1), 2),
       8, Leaf, 1),
     3, Node (Node (Leaf, 18, Leaf, 1), 9, Leaf, 1), 2),
   2, Node (Leaf, 3, Leaf, 1), 2),
 1,
 Node (Node (Node (Node (Leaf, 7, Leaf, 1), 6, Leaf, 1), 5, Leaf, 1), 4,
  Leaf, 1),
 2)
);;


let a = add 1 empty;;
assert(a = Node(Leaf, 1, Leaf, 1));;

let aa co n doCzego = 
    let rec pom i acc = 
        if i < n then pom (i+1) (add co acc) else acc
    in pom 0 doCzego;;

let g = aa 1 10 empty;;
let h = aa 2 10 g;;
assert(is_empty h = false);;
*)