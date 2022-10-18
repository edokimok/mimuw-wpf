(*********** PRZELEWANKA ***********)
(********** Emilia Dębicka *********)
(** sprawdzający: Marek Lisowski ***)

exception Exit of int;; 

let przelewanka (pom_arr : (int * int) array) =
    (* usuwam z tablicy pom_arr szklanki o pojemności 0 i tworzę nową tablicę arr *)
    let pom_list = Array.to_list pom_arr in
    let pom_list2 = List.filter (fun (x, _) -> not (x = 0)) pom_list in
    let arr = Array.of_list pom_list2 in

    let n = Array.length arr in

    let mx = Array.init n (fun i -> fst arr.(i)) in (* tablica pojemności *)
    let res = Array.init n (fun i -> snd arr.(i)) in (* tablica oczekiwanych objętości *)

    let q = Queue.create () in

    (* funkcja przelewająca wodę ze szklanki o indeksie [j] do [i] *)
    let add i j cur =
        let new_cur = Array.copy cur in
        if cur.(i) + cur.(j) <= mx.(i) then
            begin
                new_cur.(i) <- cur.(i) + cur.(j);
                new_cur.(j) <- 0;
            end
        else
            begin
                new_cur.(i) <- mx.(i);
                new_cur.(j) <- cur.(i) + cur.(j) - mx.(i);
            end;
        new_cur
    in

    (* funkcja przelewająca dokładnie x wody do szklanki o indeksie [i] *)
    let add_exact i x cur =
        let new_cur = Array.copy cur in
        if cur.(i) + x <= mx.(i) then
            new_cur.(i) <- cur.(i) + x
        else
            begin
                new_cur.(i) <- mx.(i);
            end;
        new_cur
    in

    (* tablica odwiedzonych *)
    let visited = Hashtbl.create 10009 in

    (* funkcja odznaczająca nieodwiedzone stany *)
    let visit state cnt =
        if Hashtbl.mem visited state then ()
        else
            begin
                Hashtbl.add visited state cnt;
                Queue.push state q;
            end
    in

    (* funkcja pomocnicza generująca kolejne możliwe stany poziomów wody w szklankach *)
    let generate_states cur cnt =
        for i = 0 to n-1 do
            if cur.(i) = mx.(i) then
                visit (add_exact i (-cur.(i)) cur) cnt
            else if cur.(i) = 0 then
                begin
                    visit (add_exact i mx.(i) cur) cnt;
                    for j = 0 to n-1 do
                        if i = j || cur.(j) = 0 then ()
                        else
                            visit (add i j cur) cnt
                    done
                end
            else
                begin
                    visit (add_exact i mx.(i) cur) cnt; (* napełnianie szklanki *)
                    visit (add_exact i (-cur.(i)) cur) cnt; (* wylewanie wody ze szklanki *)
                    for j = 0 to n-1 do
                        if i = j || cur.(j) = 0 then ()
                        else
                            visit (add i j cur) cnt
                    done
                end
        done
    in

    let rec bfs cur =
        let cnt = Hashtbl.find visited cur in
        if Array.for_all2 (fun x y -> x = y) cur res then
            raise (Exit cnt)
        else
            begin
                generate_states cur (cnt + 1);
                if Queue.is_empty q then raise (Exit (-1))
                else
                    begin
                        let s = Queue.pop q in
                        bfs s
                    end
            end
    in

    (* obecnie rozpatrywany stan *)
    let cur = Array.make n 0 in
    visit cur 0;

    (* obliczanie nwd *)
    let check_nwd =
        let rec nwd a b =
            if b = 0 then a
            else nwd b (a mod b)
        in
        let gcd = Array.fold_left (fun a x -> nwd a x) 0 mx in
        if gcd = 0 then true else
        Array.for_all (fun a -> a mod gcd = 0) res
    in

    (* sprawdzanie czy wynik jest spełnialny, ponieważ po każdym ruchu jakaś szklanka musi być pełna lub pusta *)
    let check_empty_or_full =
        n = 0 || Array.fold_left (fun a (x, y) -> a || y = 0 || x = y) false arr
    in

    if not check_nwd || not check_empty_or_full then -1
    else
        try bfs cur with
            | Exit i   -> i
            | _         -> failwith "yyy"
;;

(* przelewanka [|(7, 2); (10, 3)|];;
let c = [|(10,2);(20,20);(10,0);(1000,1000)|];;
assert ( przelewanka c = -1 );; *)

