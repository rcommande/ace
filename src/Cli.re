open Cmdliner;
open Base;

let shell_t = Term.(const(_ => Ace_shell.Shell.run()) $ const());
let serve_t = Term.(const(_ => Ace_web.Server.start_server()) $ const());

let default_cmd = (shell_t, Term.info("shell"));
let run_cmd = (serve_t, Term.info("run"));
let cmds = [default_cmd, run_cmd];

let () = Term.(exit @@ Term.eval_choice(default_cmd, cmds));
