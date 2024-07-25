type t = {
  price : float
}

let formatted_expedia_address city_1 city_2 day month year = 
  "https://www.expedia.com/Flights-Search?flight-type=on&mode=search&trip=oneway&leg1=" ^
  "from%3A" ^ city_1 ^ "%2C+%2Cto%3A" ^ city_2 ^ 
  "%2C+%2Cdeparture%3A" ^ month ^ "%2F" ^ day ^ "%2F" ^ year
;;


let get_credits contents  =
  let open Soup in
  parse contents
  $$ "a[class=ipc-primary-image-list-card__title]"
  |> to_list
  |> List.map (fun li -> texts li (*|> String.concat ~sep:"" |> String.strip*))
;;

(* let get_flights contents =
  let open Soup in
  parse contents
  $$ "a[class=ipc-primary-image-list-card__title]"
  |> to_list
  |> List.map ~f:(fun li -> texts li)
;; *)

