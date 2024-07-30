module W = Webdriver_cohttp_async
open! Async

let run () =
  let%bind session, _ = W.Session.make ~host:"http://localhost:9515" W.Capabilities.chrome_headless in

  print_endline "test";

  let%bind () = W.goto ~session "https://github.com/art-w/ocaml-webdriver" in
  let%bind commits =
    W.find_first
      `xpath
      "//a[@href='/art-w/ocaml-webdriver/commits/master']//strong"
      ~session
  in
  let%bind nb = W.text commits ~session in
  let nb = int_of_string nb in
  printf "number of commits = %i\n%!" nb ;
  return ()

(* let command = Command.async (W.run ~host Capabilities.firefox_headless test) *)
let command =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = 
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller"*)
        return ()
      in
     fun () ->

      run ()
      
      (* match%map Monitor.try_with run with
      | Ok () -> return ()
      | Error e ->
      
        W.Webdriver e ->
        printf "[FAIL] Webdriver error: %s\n%!" (W.Error.to_string e) ;
        Printexc.print_backtrace stderr;
        return () *)
      )
;;