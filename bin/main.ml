(* let command = Command.group ~summary:"trip advisor" ["plan", Trip_advisor_lib.Trip_advisor.command_play; "test-web-driver", Trip_advisor_lib.Tester_webdriver.command]
let () = Command_unix.run command;; *)

let () = Command_unix.run Trip_advisor_lib.Trip_advisor.command;;
