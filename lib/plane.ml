open! Core
open Async
open! Cohttp
open Cohttp_async


type t = {
  price : float
}

let formatted_expedia_address city_1 city_2 day month year = 
  "https://www.expedia.com/Flights-Search?flight-type=on&mode=search&trip=oneway&leg1=" ^
  "from%3A" ^ city_1 ^ "%2C+%2Cto%3A" ^ city_2 ^ 
  "%2C+%2Cdeparture%3A" ^ month ^ "%2F" ^ day ^ "%2F" ^ year
;;


let get_flights contents : string list=
  let open Soup in
  parse contents
  $$ "li[data-test-id=offer-listing]"
  |> to_list
  |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |> String.strip)
;;


(* let config_distance_address ?(waypoints = "") place_id_origin place_id_destination transit_mode = 
  "https://maps.googleapis.com/maps/api/directions/json?destination=place_id:" ^ place_id_destination ^
  "&mode=" ^ transit_mode ^ 
  "&origin=place_id:" ^ place_id_origin ^ waypoints
;; *)
let create_kayak_header () = 
  Cohttp.Header.of_list [
    ("user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36")
  ]
    (* ("cookie" , "Apache=QNfpKA-AAABkPAqAJc-54-JdIaKQ; cluster=4; p1.med.sid=R-49$UjUsdgap25aKGpYXhR-DFgBJ8NGgaTl1_0AOiak0rxUjhTT6uWoVjE_I8WvK; kmkid=A0XkVnmyxzwqATKAwu0-r1o; kayak=zqyZOXjZB29jHcLHalg2; csid=a9d0f2b0-e96b-4d23-bb0f-6dbb5b18346f; kayak.mc=AYFEAtIwhqVB39mBd2E3HdaVDssD1ZaRyUk4YIyOW2aypduqBphiyWBN5_nAb2dT9gYrwhTyCcbHIf0WAPBfW2mCh7ElBKbl8gVqCGXEHiAiGuCUvEzZLwb7DUaZ5I6gf-H6F1Z2D9Wk22Mce7E7wudRTE7XZjh2UxwLoHZkU-9PjUaza_J1DV9iaxwb2gJm8Xq5_l6PnO1DUnMCaekanUz3jRPcGHEbpW2AVItYeTUdBOKUAR0JX-Il1ACljvWYcnEVHr46n_6ePwbbAKl9yORzm0EIykXMnrnWFoHWKF9EqbNQtfu_QVn2RCNkyncIAh79q6c76QyrBvwttA2ltATW5WslBZ9YGbQGlGJV6YpxiWy2I5JG-Wujlp4WrvnK5VKjWUn3zNEMNfyxuX2QihI; tracker_device=e811bf7f-27eb-4d61-b964-3bcb566b05f1; __gads=ID=afa3ff10f315a122:T=1722016206:RT=1722016206:S=ALNI_MasHmGhOuN3UnqpkweGLhNNXMyYcw; __gpi=UID=00000eb47bcbcf73:T=1722016206:RT=1722016206:S=ALNI_MasKIeQaFeMNRdE7-AXY9ORSfP5QQ; __eoi=ID=1fa5528f93de2d3c:T=1722016206:RT=1722016206:S=AA-AfjZgTHc4KkFgAGiibsgXamg4; _gcl_au=1.1.248225124.1722016218; _fbp=fb.1.1722016217000.0.8182493052174078; _yoid=5774ca9c-f9b4-44c3-831a-89ffb90a667b; _yosid=0688e58f-a552-4bd4-af90-68364ca75cc7; _tt_enable_cookie=1; _ttp=HMtyYNTYt2n-zaO9i8PaparRawq; mst_iBfK2g=Ewhj3dMNfdxtyoCLZEyejNRu3rQ6LaqbvTZhHFdp-Jr6AuyPv5vpbOmJjplssGRgW2Vr9CIZFdeqGD4jrwdzCQ; _uetsid=8536fc704b7711efb292e79bc2cdf62f|oy8vdg|2|fns|0|1668; _uetvid=8536e4b04b7711ef87bb433b5d942a06|oaeq1i|1722016248401|2|1|bat.bing.com/p/insights/c/z; forterToken=91821e9621c5423f9a4b3e022a6d0852_1722016247198__UDF43-m4_21ck_; FCNEC=%5B%5B%22AKsRol_6ctmUIFHpCm3yL0DTEc8eaHuWiSeE1mnXLvy1fCsnp1MCcynK7COqPSnm9Rpa5HnljhbccVArhRlHscrHkveJgVHTvPGUgPrAL87_HjdzPoj-eqf6r8vbihl_-2aBcd4WkmI8lXlo-Bq2AUMm8LbHIQVstA%3D%3D%22%5D%5D; mst_ADIrkw=3ECJ2qYvleX5_ymq3WP7TXURv3-PKgauxdCrZc7r8cuefCZJzq9gHMgy4F6C5Mpn_IWTWJ0wXLtcelv712212Q")] *)
;;


let call_api () =
  Client.get ~headers:(create_kayak_header ()) (Uri.of_string ("https://www.kayak.com/flights/NYC-SFO/2024-09-18?sort=bestflight_a")) >>= fun (_resp, body) ->
  let%bind body = Body.to_string body in
  (* print_endline body; *)
  return body
;;

let parse_kayak js_file = 

  let open Soup in
  let script = List.hd_exn (parse js_file
  $$ "script[id=__R9_HYDRATE_DATA__][type=application/json]"
  |> to_list
  |> List.map ~f:(fun li -> texts li |> String.concat ~sep:"" |> String.strip)) in

  let script_json = Jsonaf.of_string script in

  print_s [%sexp (script_json : Jsonaf.t)];

  (* parse js_file $$ "script[id=__R9_HYDRATE_DATA__][type=application/json]" |> R.leaf_text *)

;;


