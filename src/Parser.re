open Base;
open Re2;

let command_regex = Re2.create_exn("^(?P<command>\\!\\w+)\\ *(?P<args>.*)$");

let filter_empty_string = string_list =>
  List.filter(string_list, item => Poly.(item != ""));

let parse_command = input => {
  switch (find_submatches_exn(command_regex, input)) {
  | subs =>
    let command = Option.value(subs[1], ~default="");
    let args_sub = subs[2];
    let args =
      switch (args_sub) {
      | Some(sub) => String.split(sub, ~on=' ') |> filter_empty_string
      | None => []
      };
    Ok((command, args));
  | exception (Exceptions.Regex_match_failed(message)) => Error(message)
  };
};
