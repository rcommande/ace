open Cmdliner;
open Base;

let run_shell = _ => {
  Lwt_main.run(Shell.run_shell());
}

let shell_t = Term.(const(run_shell) $ const());

let default_cmd = (shell_t, Term.info("shell"));
let cmds = [default_cmd];

let () = Term.exit @@ Term.eval_choice(default_cmd, cmds);
