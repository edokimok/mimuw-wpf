(************ ISET ************)
(********* Emilia Dębicka ********)
(* sprawdzająca: Zuzanna Ortenburger *)

(* typ: lewe poddrzewo, przedział, prawe poddrzewo, wysokość, liczba elementów *)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int

(* operacje dodawania i odejmowania uwzględniające min_int i max_int *)
let (+*) a b =
  if a = max_int || b = max_int || (a >= 0 && b >= 0 && a + b < 0) then max_int else a + b

let (-*) a b =
  if a = min_int && b = min_int then 0 else
  if a >= 0 && b = min_int then max_int else
  if a = min_int && b >= 0 then min_int else
  if a <= 0 && b = max_int then min_int else
  if b <= 0 then a +* (abs b) else
  a - b

(* funkcje pomocnicze *)
let inside x (a, b) = (a <= x +* 1 && x -* 1 <= b)

let size (a, b) = b -* a +* 1

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let number = function
  | Node (_, _, _, _, n) -> n
  | Empty -> 0

let make l (x, y) r = Node (l, (x, y), r, max (height l) (height r) +* 1, number l +* number r +* (y -* x) +* 1)

let combine (a, b) (c, d) = (a, d)

(* funkcja pomocnicza wyrównująca drzewo do postaci wariantu drzewa AVL *)
(* nie rekurencyjna *)
let bal l (x, y) r =
  let hl = height l in
  let hr = height r in
  if hl > hr +* 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr (x, y) r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr (x, y) r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl +* 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l (x, y) rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l (x, y) rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, (x, y), r, max hl hr +* 1, number l +* number r +* (y -* x) +* 1)

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> (max_int, max_int)

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> Empty

let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> (min_int, min_int)

let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> Empty

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

let cmp (c, d) (a, b) =
  if b < c then 1
  else if d < a then -1 
  else 0

(* funkcja dodająca jeden rozłączny przedział do drzewa (z zachowaniem warunku BST i AVL) *)
let rec add_one x = function
  | Node (l, k, r, h, nr) ->
      let c = cmp x k in
      if c = 0 then Node (l, x, r, h, nr)
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, size x)


(* ================================================================================== *)

let empty = Empty

let is_empty x = 
  x = Empty

(* funkcja łącząca dwa drzewa (z zachowaniem warunku AVL) *)
(* v to przedział rozłączny z l i r *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, (lx, ly), lr, lh, ln), Node(rl, (rx, ry), rr, rh, rn)) ->
      if lh > rh +* 2 then bal ll (lx, ly) (join lr (rx, ry) r) else
      if rh > lh +* 2 then bal (join l (lx, ly) rl) (rx, ry) rr else
      bal l v r

let split x (set : t) =
  let rec pom set1 =
    match set1 with
    | Empty -> (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = cmp (x, x) v in
        if c = 0 then
          let nl = if x = fst (v) then l else bal l (fst (v), x -* 1) Empty in
          let nr = if x = snd (v) then r else bal Empty (x +* 1, snd (v)) r in
          (nl, true, nr)
        else if c < 0 then
          let (ll, check, rl) = pom l in (ll, check, join rl v r)
        else
          let (lr, check, rr) = pom r in (join l v lr, check, rr)
  in
  pom set

(* funkcja dodaje przedział do drzewa poprzez podzielenie go na dwa poddrzewa oraz ponowne złączenie, *)
(* uwzględniając nowy przedział *)
let add (c, d) set =
  let (nl, _, _) = split c set in
  let (_, _, nr) = split d set in
  if nl != Empty && nr != Empty && inside c (max_elt nl) && inside d (min_elt nr) then
    bal (remove_max_elt nl) (fst (max_elt nl), snd (min_elt nr)) (remove_min_elt nr)
  else if nl != Empty && inside c (max_elt nl) then
    bal (remove_max_elt nl) (fst (max_elt nl), d) nr
  else if nr != Empty && inside d (min_elt nr) then
    bal nl (c, snd (min_elt nr)) (remove_min_elt nr)
  else
    bal nl (c, d) nr

let remove (c, d) t =
    match t with
    | Empty -> Empty
    | Node (l, (a, b), r, _, _) ->
      let (nl, _, _) = split c t in
      let (_, _, nr) = split d t in
      merge nl nr

let below n s =
  let rec pom set acc =
    match set with
    | Empty -> acc
    | Node (l, (a, b), r, _, nr) ->
      if n >= a && n <= b then number l +* (n -* a) +* 1 +* acc
      else if n < a then pom l acc else pom r (number l +* (b -* a) +* 1 +* acc) in
  pom s 0

(* funckja pomocnicza przechodząca drzewo w dół *)
(* złożoność zależna od wysokości drzewa, w przypadku drzewa AVL O(log n) gdzie n to liczba wierzchołków *)
let rec traverse n s =
  match s with
  | Empty -> Empty
  | Node (l, (a, b), r, _, _) as tree ->
    if n >= a && n <= b then tree else if n < a then traverse n l else traverse n r

let mem n set =
  let s = traverse n set in
  match s with
  | Empty -> false
  | _ -> true

let iter f set =
  let rec pom = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> pom l; f k; pom r in
  pom set

let fold f s a =
  let rec pom a = function
    | Empty -> a
    | Node (l, k, r, _, _) ->
          pom (f k (pom a l)) r in
  pom a s

let elements set = 
  let rec pom acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> pom (k :: pom acc r) l in
  pom [] set;;


(*
let t1 = Node (Node (Empty, (0, 2), Empty, 1, 3), (4, 8), Node (Node (Empty, (13, 16), Empty, 1, 4), (20, 25), Empty, 2, 6), 3, 18);;
let t2 = add (1,3) t1;;
let t3 = add (27,28) t2;;
*)