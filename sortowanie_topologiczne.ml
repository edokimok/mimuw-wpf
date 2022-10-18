(** SORTOWANIE  TOPOLOGICZNE **)
(******* Emilia Dębicka *******)
(* sprawdzająca: Maja Tkaczyk *)

open PMap;;

(* wyjątek, informujący o cyklu w grafie *)
exception Cykliczne;;

let topol edge_list =
    (* [map1] : wartość - identyfikator, [map2] : identyfikator - wartość *)
    (* aby móc używać funkcji [find] w obie strony *)
    let map1 = ref empty in
    let map2 = ref empty in
    let pom = ref [] in
    
    (* tworzymy listę wszystkich wierzchołków [vertices] *)
    let vertices = ref [] in
    List.fold_left (fun _ (x, lst) -> vertices := x::!vertices;
                                      List.fold_left (fun _ x -> vertices := x::!vertices) () lst) () edge_list;
    
    (* przyporządkowujemy wartości identyfikatorom przy pomocy modułu PMap *)
    let id = ref 1 in
    List.fold_left (fun _ e ->
        if not (exists e !map1) then
            begin
                map1 := add e !id !map1;
                map2 := add !id e !map2;
                id := !id + 1;
            end
        else ()
    ) () !vertices;

    (* [n] - ilość różnych wierzchołków w grafie *)
    let n = !id-1 in
    let graph = Array.make (n+9) [] in

    (* wierzchołek może być w trzech stanach: 0 - nieodwiedzony, 1 - przetwarzany, 2 - odwiedzony *)
    let state = Array.make (n+9) 0 in
    
    (* tworzymy listę sąsiadów dla każdego wierzchołka *)
    List.fold_left (fun _ (x, lst) ->
        graph.(find x !map1) <- graph.(find x !map1)@(List.fold_left (fun acc el ->
            let y = find el !map1 in
                y::acc
        ) [] lst);
    ) () edge_list;

    let rec dfs i neighbours =
        if state.(i) = 0 then
            begin
                state.(i) <- 1;
                List.fold_left (fun _ x ->
                    if state.(x) = 1 then raise Cykliczne
                    else if state.(x) = 0 then
                        dfs x graph.(x)
                    else ()
                ) () neighbours;
                state.(i) <- 2;
                pom := i::(!pom)
            end
        else ()
    in

    for i = 1 to n do
        dfs i graph.(i)
    done;

    (* z powrotem konwertujemy indentyfikatory na pierwotne wartości *)
    List.rev (List.fold_left (fun acc nr ->
        (find nr !map2)::acc
    ) [] !pom);;


(* topol [(1, [2]); (1, [3])];;
topol [(1, [2; 6; 4]); (2, []); (3, [8; 4]); (4, []); (5, [1]); (6, []); (7, [1; 8]); (8, [6])];;
topol [(10, [20; 30]); (20, [30])];;
topol [('a', ['b'; 'f'; 'd']); ('b', []); ('c', ['h'; 'd']); ('d', []); ('e', ['a']); ('f', []); ('g', ['a'; 'h']); ('h', ['f'])];; *)