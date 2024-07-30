open! Core 

module type Weight = sig 
  type t
include Comparable.S with type t := t
val zero : t
val (+) : t -> t -> t

end

module Make_tsp (Weight : Weight) = struct 


let rec small_input_search ~cur_path ~cur_time ~(dest_set : Location.Set.t) ~(path_map : Weight.t Location.Table.t Location.Table.t) = 
  if Set.is_empty dest_set then (cur_path, cur_time) else 
    Set.fold ~init:(cur_path, None) dest_set ~f:(fun (best_path, shortest_time) dest -> 
      let origin = List.last_exn cur_path in
      let edge_map = Hashtbl.find_exn path_map origin in
        let travel_time = Hashtbl.find_exn edge_map dest in
        let (check_path, check_time) = small_input_search ~cur_path:(cur_path @ [dest]) ~cur_time:(Weight.(+) cur_time travel_time) ~dest_set:(Set.remove dest_set dest) ~path_map in
        match shortest_time with 
        | None -> (check_path, Some check_time)
        | Some shortest_time ->
       ( if Weight.(<) check_time shortest_time then (check_path, Some check_time) else (best_path, Some shortest_time))
    ) |> Tuple2.map_snd ~f:(fun time -> Option.value_exn time)
;;

let rec _atps_optimized_tsp ~cur_path ~cur_time ~dest_set ~(path_map : Time_ns.Span.t Location.Table.t Location.Table.t) = 
  if Set.is_empty dest_set then (cur_path, cur_time) else 
    Set.fold ~init:(cur_path, Time_ns.Span.max_value_representable) dest_set ~f:(fun (best_path, shortest_time) dest -> 
      let origin = List.last_exn cur_path in
      let edge_map = Hashtbl.find_exn path_map origin in
        let travel_time = Hashtbl.find_exn edge_map dest in
        let (check_path, check_time) = _atps_optimized_tsp ~cur_path:(cur_path @ [dest]) ~cur_time:(Time_ns.Span.(+) cur_time travel_time) ~dest_set:(Set.remove dest_set dest) ~path_map in
        if Time_ns.Span.(<) check_time shortest_time then (check_path, check_time) else (best_path, shortest_time)
    )
;;

let rec stsp_optimized_tsp ~cur_path ~cur_time ~(dest_set : Location.Set.t) ~(path_map : Time_ns.Span.t Location.Table.t Location.Table.t) = 
  match Set.is_empty dest_set with 
  | true ->  (cur_path, cur_time)
  | false ->
    let origin = List.last_exn cur_path in
    let edge_map = Hashtbl.find_exn path_map origin in
    let shortest_path = Set.fold ~init:(origin, Time_ns.Span.max_value_representable) dest_set ~f:(fun (closest_dest, time) dest -> 

      let dest_time = Hashtbl.find_exn edge_map dest in
      match Time_ns.Span.(<) time (dest_time) with
      | true -> (closest_dest, time)
      | false -> (dest, dest_time)
      
      ) in
    let nearest_neighbor = Tuple2.get1 shortest_path in
    let path_time = Tuple2.get2 shortest_path in
    stsp_optimized_tsp ~cur_path:(cur_path @ [nearest_neighbor]) ~cur_time:(Time_ns.Span.(+) cur_time path_time) ~dest_set:(Set.remove dest_set nearest_neighbor) ~path_map:path_map
;;

let get_shortest_path ~origin ~(dest_list : Location.t list) ~(path_map : Time_ns.Span.t Location.Table.t Location.Table.t) : (Location.t list * Time_ns.Span.t) =
  let dest_set = Location.Set.of_list dest_list in
  if List.length dest_list <= 10 then
    small_input_search ~cur_path:[origin] ~cur_time:Time_ns.Span.zero ~dest_set ~path_map
  else 
    stsp_optimized_tsp ~cur_path:[origin] ~cur_time:Time_ns.Span.zero ~dest_set ~path_map;
  (* ( print_endline "Too many locations I'm not doing allat";
    ([], Time_ns.Span.zero)
  ) *)
;;

end