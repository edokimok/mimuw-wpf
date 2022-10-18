(************ ORIGAMI ************)
(********* Emilia Dębicka ********)
(* sprawdzający: Grzegorz Gwóźdź *)


(* przbliżenie, pozwalające na porównywanie floatów *)
let eps = 0.000000000001;;

(* punkt na płaszczyźnie *)
type point = float * float

(* ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

let square = fun x -> x *. x

(* funkcja pomocnicza licząca iloczyn wektorowy *)
let vector_product (p0 : point) (p1 : point) (p2 : point) =
    (fst p1 -. fst p0)*.(snd p2 -. snd p0) -. (fst p2 -. fst p0)*.(snd p1 -. snd p0)

(* [prostokat p1 p2] zwraca prostokątną kartkę o bokach równoległych do osi układu współrzędnych
   [p1] - lewy dolny róg, [p2] - prawy górny róg *)
let prostokat (p1 : point) (p2 : point) : kartka =
    fun ((x, y) : point) ->
        if fst p1 <= x && x <= fst p2 && snd p1 <= y && y <= snd p2
            then 1
            else 0

(* [kolko p r] zwraca okrągłą kartkę o środku w punkcie [p] i promieniu [r] *)
let kolko (p : point) r : kartka =
    fun ((x, y) : point) ->
        if square (fst p -. x) +. square (snd p -. y) -. square r <= eps
            then 1
            else 0

(* funkcja zwracająca punkt symetryczny do (xa, ya) względem prostej
   przechodzącej przez punkty (x1, y1) i (x2, y2) *)
let mirror ((x1, y1) : point) ((x2, y2) : point) ((xa, ya) : point) : point =
    let al = (y2 -. y1) /. (x2 -. x1) in
        if al = infinity || al = neg_infinity then
            (2. *. x1 -. xa, ya) else
    let bl = y2 -. al *. x2 in
    let am = (x1 -. x2) /. (y2 -. y1) in
        if am = infinity || am = neg_infinity then
            (xa, 2. *. y1 -. ya) else
    let bm = ya -. am *. xa in
    let xs = (bl -. bm) /. (am -. al) in
        if xs = infinity || xs = neg_infinity then
            (2. *. x1 -. xa, ya) else
    let ys = am *. xs +. bm in
    (2. *. xs -. xa, 2. *. ys -. ya)

(* funkcja składająca składa kartkę [k] wzdłuż prostej przechodzącej przez
   punkty [p1] i [p2] z prawej na lewą (patrząc w kierunku od [p1] do [p2]) *)
let zloz ((x1, y1) : point) ((x2, y2) : point) (k : kartka) : kartka =
    fun ((xa, ya) : point) ->
        let vp = vector_product (x1, y1) (x2, y2) (xa, ya) in
        if Float.abs (vp -. 0.) <= eps then k (xa, ya)
        else if vp < 0. then 0
        else let (xb, yb) = mirror (x1, y1) (x2, y2) (xa, ya)
            in k (xa, ya) + k (xb, yb)

(* funkcja składa kartkę k kolejno wzdłuż prostych z listy *)
let skladaj lst (k : kartka) : kartka =
    List.fold_left (fun k1 (p1, p2) -> zloz p1 p2 k1) k lst;;