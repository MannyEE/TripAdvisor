open! Core 
open Async

module type S = sig 

  type weight
  type node
  type additional_weight_arg
  val make_destination_graph :
    (node, (node, weight) Hashtbl.t) Hashtbl.t ->
    node list ->
    additional_weight_arg ->
    (node, (node, weight) Hashtbl.t) Hashtbl.t Deferred.t

  val get_shortest_path :
  origin:node  ->
  dest_list:node list ->
  path_map: (node, (node, weight) Hashtbl.t) Hashtbl.t ->
  node list * weight
end


module type Arg = sig 
  module Weight: sig
    type t
    val (<) : t -> t -> bool
    val zero : t
    val (+) : t -> t -> t
  end

  module Node : sig
    type t
    include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
  end

  module Additional_weight_arg : sig 
    type t

  end
  val compute_weight : Node.t -> Node.t -> Additional_weight_arg.t -> Weight.t Deferred.t


end

module Make_tsp (Arg : Arg) : S with type weight = Arg.Weight.t and type node = Arg.Node.t and type additional_weight_arg = Arg.Additional_weight_arg.t = struct 
  
  type weight = Arg.Weight.t
  type node = Arg.Node.t
  type additional_weight_arg = Arg.Additional_weight_arg.t
  
  module Weight = Arg.Weight
  module Node = Arg.Node
    


let rec small_input_search ~cur_path ~cur_time ~(dest_set : Node.Set.t) ~(path_map : Weight.t Node.Table.t Node.Table.t) = 
  if Set.is_empty dest_set then (
    let last_loc = List.last_exn cur_path in
    let first_loc = List.hd_exn cur_path in
    let final_path = cur_path @ [first_loc] in
    let edge_map = Hashtbl.find_exn path_map last_loc in
    let final_time = Hashtbl.find_exn edge_map first_loc in
    (final_path, final_time) 
  ) else 
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

(* let rec _atps_optimized_tsp ~cur_path ~cur_time ~dest_set ~(path_map : Weight.t Location.Table.t Location.Table.t) = 
  if Set.is_empty dest_set then (cur_path, cur_time) else 
    Set.fold ~init:(cur_path, Time_ns.Span.max_value_representable) dest_set ~f:(fun (best_path, shortest_time) dest -> 
      let origin = List.last_exn cur_path in
      let edge_map = Hashtbl.find_exn path_map origin in
        let travel_time = Hashtbl.find_exn edge_map dest in
        let (check_path, check_time) = _atps_optimized_tsp ~cur_path:(cur_path @ [dest]) ~cur_time:(Time_ns.Span.(+) cur_time travel_time) ~dest_set:(Set.remove dest_set dest) ~path_map in
        if Time_ns.Span.(<) check_time shortest_time then (check_path, check_time) else (best_path, shortest_time)
    )
;; *)

let rec stsp_optimized_tsp ~cur_path ~cur_time ~(dest_set : Node.Set.t) ~(path_map : Weight.t Node.Table.t Node.Table.t) = 
  match Set.is_empty dest_set with 
  | true ->  (cur_path, cur_time)
  | false ->
    let origin = List.last_exn cur_path in
    let edge_map = Hashtbl.find_exn path_map origin in
    let shortest_path = Set.fold ~init:(origin, None) dest_set ~f:(fun (closest_dest, time) dest -> 

      let dest_time = Hashtbl.find_exn edge_map dest in
      match time with 
        | None -> (dest, Some dest_time)
        | Some time -> 
      (
        match Weight.(<) time (dest_time) with
        | true -> (closest_dest, Some time)
        | false -> (dest, Some dest_time)
      )
      ) 
    in
    let nearest_neighbor = Tuple2.get1 shortest_path in
    let path_time = Tuple2.get2 shortest_path |> Option.value_exn in
    stsp_optimized_tsp ~cur_path:(cur_path @ [nearest_neighbor]) ~cur_time:(Weight.(+) cur_time path_time) ~dest_set:(Set.remove dest_set nearest_neighbor) ~path_map:path_map
;;

let get_shortest_path ~origin ~(dest_list : Node.t list) ~(path_map : Weight.t Node.Table.t Node.Table.t) : (Node.t list * Weight.t) =
  let dest_set = Node.Set.of_list dest_list in
  if List.length dest_list <= 10 then
    small_input_search ~cur_path:[origin] ~cur_time:Weight.zero ~dest_set ~path_map
  else 
    stsp_optimized_tsp ~cur_path:[origin] ~cur_time:Weight.zero ~dest_set ~path_map;
  (* ( print_endline "Too many locations I'm not doing allat";
    ([], Time_ns.Span.zero)
  ) *)
;;


let make_destination_graph data_map (places_list : Node.t list) additional_arg = 

  let%bind () = Deferred.List.iter ~how:`Parallel places_list ~f:(fun origin_city_code -> 

    let destination_table = Hashtbl.find_or_add data_map origin_city_code ~default:Node.Table.create in
    Deferred.List.iter ~how:`Parallel places_list ~f:(fun destination_city_code ->
      match [%compare.equal:Node.t] origin_city_code destination_city_code with 
      | true -> return ()
      | false -> 
        let cur_data = Hashtbl.find destination_table destination_city_code in
        match cur_data with 
        | None ->   
          let%bind data = Arg.compute_weight origin_city_code destination_city_code additional_arg in
          Hashtbl.add_exn ~key:destination_city_code ~data destination_table;
          return ()
        | Some _ ->
          return()
    ) 
  ) in

  return data_map
;;



end


module Intra_city_duration = Make_tsp (struct 
  module Weight = Time_ns.Span
  module Node = Location 
  module Additional_weight_arg = String

  let compute_weight (origin : Location.t) (destination : Location.t) (transport_mode : string) : Weight.t Deferred.t = 
    Google_api.destination_api ~origin ~destination transport_mode
  end)

module Flight_duration = Make_tsp (struct 
  module Weight = Kayak_data.Comparing_duration
  module Node = Airport
  module Additional_weight_arg = Date

  let compute_weight (origin_airport : Airport.t) (destination_airport : Airport.t) (date : Date.t) : Weight.t Deferred.t = 
    let origin_city_code = origin_airport.code in 
    let destination_city_code = destination_airport.code in 
     Plane.plane_api ~origin_city_code ~destination_city_code ~date
  end)

module Flight_prices = Make_tsp (struct 
  module Weight = Kayak_data.Comparing_price
  module Node = Airport
  module Additional_weight_arg = Date

  let compute_weight (origin_airport : Airport.t) (destination_airport : Airport.t) (date : Date.t) : Weight.t Deferred.t = 
    let origin_city_code = origin_airport.code in 
    let destination_city_code = destination_airport.code in 
    Plane.plane_api ~origin_city_code ~destination_city_code ~date 
     
  end)


  