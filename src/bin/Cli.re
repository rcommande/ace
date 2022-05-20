open Cmdliner;
open Base;

let shell_t = Term.(const(_ => Shell.run()) $ const());
let serve_t = Term.(const(_ => Serve.run()) $ const());

let default_cmd = (shell_t, Term.info("shell"));
let server_cmd = (serve_t, Term.info("serve"));
let cmds = [default_cmd, server_cmd];

let () = Term.(exit @@ Term.eval_choice(default_cmd, cmds));
