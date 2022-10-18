(************ ARYTMETYKA ************)
(********** Emilia Dębicka **********)
(* sprawdzający: Ignacy Stępniewski *)

type wartosc = {left : float; right : float; flag : bool};;
(* typ wartosc składa się z trzech parametrów: pierwszy, typu float,            *)
(* to lewy koniec przedziału możliwych wartości, drugi float to prawy koniec,   *)
(* a trzeci parametr typu bool określa, czy bierzemy zbiór czy jego dopełnienie *)
(* flag = true gdy zbiór i flag = false gdy dopełnienie                         *)


(* funkcja pomocnicza pomagająca tworzyć wynik typu wartosc *)
let value a b c : wartosc = {left = a; right = b; flag = c};;

(* funkcje pomocnicze minimum i maximum *)
let min4 a b c d =
    ((min (min a b) (min c d)) : float);;

let max4 a b c d =
    ((max (max a b) (max c d)) : float);;

(* funkcja pomocnicza sprawdzająca, czy przedział jest pusty *)
let is_nan a =
    Float.classify_float a.left = FP_nan || Float.classify_float a.right = FP_nan;;

(* funkcja pomocnicza do funkcji razy, uwzględniająca mnożenie otwartych przedziałów razy 0 *)
let multiply a b =
    match (Float.classify_float a, Float.classify_float b) with
    |(FP_zero, FP_infinite) -> 0. 
    |(FP_infinite, FP_zero) -> 0.
    |(_, _) -> a *. b;;




(************* konstruktory *************)

let wartosc_dokladnosc (x : float) (p : float) =
    let x1 = x -. Float.abs(p /. 100. *. x) in
    let x2 = x +. Float.abs(p /. 100. *. x) in
    value x1 x2 true;;

let wartosc_od_do (x : float) (y : float) =
    value x y true;;

let wartosc_dokladna (x : float) =
    value x x true;;



(************** selektory ***************)

let rec in_wartosc (x : wartosc) (y : float) =   
    match x.flag with
        |true   -> y >= x.left && y <= x.right
        |false  -> in_wartosc (value neg_infinity x.left true) y || in_wartosc (value x.right infinity true) y;;

let min_wartosc (x : wartosc) =
    match x.flag with
        |true   -> x.left
        |false  -> neg_infinity;;

let max_wartosc (x : wartosc) =
    match x.flag with
        |true   -> x.right
        |false  -> infinity;;

let sr_wartosc (x : wartosc) =
    ((min_wartosc x) +. (max_wartosc x)) /. 2.;;
    


(************* modyfikatory *************)

(* funkcja pomocnicza łącząca dwa przedziały przy założeniu, że na wejściu dostanie przedział   *)
(* a) <neg_infinity, _> lub <_, infinity>                                                       *)
(* b) <_, 0> lub <0, _>                                                                         *)
(* c) <nan, _> lub <_, nan>                                                                     *)
let rec merge a b =
    if is_nan a || is_nan b then value nan nan true else

    let newRight = max a.left b.left in
    let newLeft = min a.right b.right in

    match (a.flag, b.flag) with
    |(true, true)   ->
        if in_wartosc a 0. && in_wartosc b 0. then
            value (min a.left b.left) (max a.right b.right) true
        else
            if newRight <= newLeft then
                value neg_infinity infinity true
            else
                value newLeft newRight false
    |(false, false) ->
        value newRight newLeft false
    |(false, true)  ->
        if a.left <= b.left then
            merge (value neg_infinity b.right true)
                (value a.right infinity true)
        else
            merge (value neg_infinity a.right true)
                (value b.left infinity true)
    |(true, false)  -> merge b a;;

(* funkcja pomocnicza do dzielenia obliczająca 1 / przedział    *)
let rec reverse a =
    if a.flag then
        match (a.left, a.right) with
        |(0., 0.)   -> value nan nan true
        |(0., x)    -> value (1. /. x) infinity true
        |(x, 0.)    -> value neg_infinity (1. /. x) true
        |(_, _)     ->
            if in_wartosc a 0. then
                value (1. /. a.left) (1. /. a.right) false
            else
                value (1. /. a.right) (1. /. a.left) true
    else
        merge (reverse (value neg_infinity a.left true))
            (reverse (value a.right infinity true));;



let rec plus (a : wartosc) (b : wartosc) =
    match (a.flag, b.flag) with
    |(true, true) -> value (a.left +. b.left) (a.right +. b.right) true
    |(false, _) -> merge (plus (value neg_infinity a.left true) b)
                            (plus (value a.right infinity true) b)
    |(_, false) -> plus b a;;


let minus (a : wartosc) (b : wartosc) =
    plus (a : wartosc) (value (b.right *. -1.) (b.left *. -1.) b.flag);;


let rec razy (a : wartosc) (b : wartosc) =
    match (a.flag, b.flag) with
    |(true, true) ->
        let w1 = multiply a.left b.left in
        let w2 = multiply a.left b.right in
        let w3 = multiply a.right b.left in
        let w4 = multiply a.right b.right in
        value (min4 w1 w2 w3 w4) (max4 w1 w2 w3 w4) true 
    |(false, _) ->
        merge (razy (value neg_infinity a.left true) b)
              (razy (value a.right infinity true) b)
    |(_, false) -> razy b a;;


let podzielic (a : wartosc) (b : wartosc) =
    razy a (reverse b);;