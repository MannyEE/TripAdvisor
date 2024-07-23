open! Core
open Async

let rec get_desired_places ((): unit) = 
  let%bind input = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter destination" in
  match String.equal input "" with 
    | true -> 
      return [];
    | false -> 
      let%map places_list = get_desired_places () in
      input :: places_list
;;

let make_destination_graph places_list transport_mode = 
  let map = Hashtbl.create (module String) in
  let%bind _ = Deferred.List.iter ~how:`Parallel places_list ~f:(fun origin -> 
    let empty_tbl = Hashtbl.create (module String) in
    Hashtbl.add_exn map ~key:origin ~data:empty_tbl;

    Deferred.List.iter ~how:`Parallel places_list ~f:(fun destination ->
      match String.equal origin destination with 
      | true -> return ()
      | false -> 
        let%bind distance_in_seconds = Google_api.destination_api origin destination transport_mode in
        let distance = (distance_in_seconds) / 60 in
        Hashtbl.add_exn (Hashtbl.find_exn map origin) ~key:destination ~data:distance;
        return()
    ) 
  ) in
  return map
;;

let run () = 
  let%bind origin_address = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter origin location" in

  print_endline "What places would you like to visit? Put in one address at a time";
  let%bind places_list = get_desired_places () in
  let all_places = origin_address :: places_list in

  let%bind travel_method = Async_interactive.ask_dispatch_gen ~f:(fun input -> Ok input) "Enter travel method" in
  let%bind _graph = make_destination_graph (List.dedup_and_sort all_places ~compare:String.compare) travel_method in

  (* print_s [%sexp (graph : string String.Table.t String.Table.t)]; *)
  return ();
;;


let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
    in
    fun () -> 
      let%bind _f = run () in
      return ()
    )
;;

let () = Command_unix.run command_play;;


(* let run = 
    let lat_and_long = 
      Jsonaf.member_exn "results" coordinates 
    |> Jsonaf.list_exn |> List.hd_exn |> Jsonaf.member_exn "geometry"|> Jsonaf.member_exn "location" in
    let lat = Jsonaf.float_exn (Jsonaf.member_exn "lat" lat_and_long) in
    let long = Jsonaf.float_exn (Jsonaf.member_exn "lng" lat_and_long) in

    let direction_info = 
      Jsonaf.member_exn "routes" direction_info |> Jsonaf.list_exn |>  List.hd_exn |> Jsonaf.member_exn "legs" |> Jsonaf.list_exn |> List.hd_exn |>  Jsonaf.member_exn "duration" |> Jsonaf.member_exn "value" in
    let duration = Jsonaf.int_exn direction_info in *)
