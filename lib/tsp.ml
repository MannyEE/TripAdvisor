open! Core 

let rec small_input_search ~cur_path ~cur_time ~dest_set ~(path_map : Time_ns.Span.t Location.Table.t Location.Table.t) = 
  if Set.is_empty dest_set then (cur_path, cur_time) else 
    Set.fold ~init:(cur_path, Time_ns.Span.max_value_representable) dest_set ~f:(fun (best_path, shortest_time) dest -> 
      let origin = List.last_exn cur_path in
      let edge_map = Hashtbl.find_exn path_map origin in
        let travel_time = Hashtbl.find_exn edge_map dest in
        let (check_path, check_time) = small_input_search ~cur_path:(cur_path @ [dest]) ~cur_time:(Time_ns.Span.(+) cur_time travel_time) ~dest_set:(Set.remove dest_set dest) ~path_map in
        if Time_ns.Span.(<) check_time shortest_time then (check_path, check_time) else (best_path, shortest_time)
    )
;;

let get_shortest_path ~origin ~(dest_list : Location.t list) ~(path_map : Time_ns.Span.t Location.Table.t Location.Table.t) : (Location.t list * Time_ns.Span.t) =
  if List.length dest_list <= 10 then
  (let dest_set = Location.Set.of_list dest_list in
  small_input_search ~cur_path:[origin] ~cur_time:Time_ns.Span.zero ~dest_set ~path_map)
  else 
  ( print_endline "Too many locations I'm not doing allat";
    ([], Time_ns.Span.zero)
  )
;;