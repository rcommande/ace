open Base;
open Core.Types;
open Core.Types.Action;
open Lwt;
open Re2;

let command_regex = Re2.create_exn("^\\!(?P<command>\\w+)\\ *(?P<args>.*)$");

exception Command_execution_fail;

let filter_empty_string = string_list =>
  List.filter(string_list, ~f=item => Poly.(item != ""));

type error =
  | InvalidCommand(string, string);

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

let process_input = input =>
  if (String.is_prefix(input, ~prefix="!")) {
    switch (parse_command(input)) {
    | Ok((name, args)) => Ok(Input.Command(input, name, args))
    | Error(message) => Error(InvalidCommand(input, message))
    };
  } else {
    Ok(Input.Sentence(input));
  };

let build_action_event_array = (actions: array(Action.t)) => {
  Array.map(actions, ~f=action =>
    List.map(action.on, ~f=event => (action, event)) |> List.to_array
  )
  |> Array.to_list
  |> Array.concat;
};

let find_action_or_default =
    (incoming: Incoming.t, actions: array(Action.t), default: Action.t) => {
  let found_opt = {
    actions
    |> build_action_event_array
    |> Array.find(~f=(item: (Action.t, Event.t)) => {
         switch (item, incoming.input) {
         | ((action, Command(event_name)), Command(_, input_name, _))
             when Poly.(input_name == event_name) =>
           true
         | _ => false
         }
       });
  };
  switch (found_opt) {
  | Some((action, event)) => (action, Some(event))
  | None => (default, None)
  };
};

let execute_directreponse = (incoming: Incoming.t) => {
  switch (incoming.input) {
  | Input.Command(_, name, _) when Poly.(name == "ping") =>
    Lwt_result.return(Response.Ok_)
  | Input.Command(_, name, _) when Poly.(name == "unknown") =>
    Lwt_result.return(Response.Error("Unknown command"))
  | _ => Lwt_result.fail("Command execution failed")
  };
};

let execute =
    (incoming: Incoming.t, event: option(Event.t), action: Action.t) => {
  let thread =
    switch (action.runner) {
    | Runner.DirectResponse => execute_directreponse(incoming)
    | Runner.HttpResponse(url, method, allowed, params, headers) =>
      HTTPResponse.execute_http_request(url, method, allowed, params, headers)
    };
  thread
  >>= (
    response_res => {
      switch (response_res) {
      | Ok(response) =>
        let outgoing = Outgoing.{action, response, event};
        Lwt_result.return(Interaction.make(incoming, outgoing));
      | Error(message) => Lwt_result.fail(message)
      };
    }
  );
};

let run_command = (text, actions, default_action, origin, destination) => {
  let input_res = process_input(text);
  switch (input_res) {
  | Ok(input) =>
    let incoming = Incoming.{input, origin, destination};
    let (action, event_opt) =
      find_action_or_default(incoming, actions, default_action);
    execute(incoming, event_opt, action);
  | Error(message) => Lwt.fail(Command_execution_fail)
  };
};
