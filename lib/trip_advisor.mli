val get_desired_places : unit -> string list Async.Deferred.t
val print_optimal_route :
  origin:Location.t ->
  location_list:Location.t list ->
  day:int ->
  distance_data:Core.Time_ns.Span.t Tsp.Intra_city_duration.Node.Table.t
                Tsp.Intra_city_duration.Node.Table.t ->
  unit Async.Deferred.t
val get_best_route : 'a -> 'b -> 'c
val run : unit -> unit Async.Deferred.t
val command : Command.t
