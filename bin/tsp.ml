open! Core 

let rec small_input_search ~cur_path ~cur_time ~dest_set ~(path_map : int String.Table.t String.Table.t) = 
  if Set.is_empty dest_set then (cur_path, cur_time) else 
    Set.fold ~init:(cur_path, Int.max_value) dest_set ~f:(fun (best_path, shortest_time) dest -> 
      let origin = List.last_exn cur_path in
      let edge_map = Hashtbl.find_exn path_map origin in
        let travel_time = Hashtbl.find_exn edge_map dest in
        let (check_path, check_time) = small_input_search ~cur_path:(cur_path @ [dest]) ~cur_time:(cur_time + travel_time) ~dest_set:(Set.remove dest_set dest) ~path_map in
        if check_time < shortest_time then (check_path, check_time) else (best_path, shortest_time)
    )
;;

let get_shortest_path ~origin ~dest_list ~(path_map : int String.Table.t String.Table.t) : (string list * int) =
  if List.length dest_list <= 6 then
  (let dest_set = String.Set.of_list dest_list in
  small_input_search ~cur_path:[origin] ~cur_time:0 ~dest_set ~path_map)
  else 
  ( print_endline "Too many locations I'm not doing allat";
    ([], 0)
  )
;;