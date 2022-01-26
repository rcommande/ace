open Base;

module Origin = {
  type t =
    | All
    | Slack
    | Event
    | Shell;

  let to_string = origin => {
    switch (origin) {
    | All => "all"
    | Slack => "slack"
    | Event => "event"
    | Shell => "shell"
    };
  };
};

module Action = {
  module Event = {
    type t =
      | Command(string, option(list(string)))
      | Unknown;

    let to_string = event => {
      switch (event) {
      | Command(_, _) => "command"
      | Unknown => "Unknown"
      };
    };
  };

  module Runner = {
    type url = string;

    type t =
      | HttpResponse(
          url,
          Cohttp.Code.meth,
          list(HTTPResponse.status_code),
          HTTPResponse.Params.t,
          HTTPResponse.Headers.t,
        )
      | DirectResponse;

    let to_string = runner => {
      switch (runner) {
      | DirectResponse => "DirectResponse"
      | HttpResponse(_) => "HttpResponse"
      };
    };
  };

  type t = {
    name: string,
    from: Origin.t,
    on: Event.t,
    runner: Runner.t,
  };
};

let find_action_or_default =
    (origin, actions: list(Action.t), command, default) => {
  let result =
    List.find(actions, ~f=action => {
      switch (action.on) {
      | Action.Event.Command(identifier, _) =>
        String.equal(identifier, command)
        && (Poly.(origin == action.from) || Poly.(action.from == Origin.All))
      | _ => false
      }
    });
  switch (result) {
  | Some(action) => action
  | None => default
  };
};

let process_input = (origin, input, actions, default) => {
  switch (Parser.parse_command(input)) {
  | Ok((command, args)) =>
    find_action_or_default(origin, actions, command, default)
  | Error(_) => default
  };
};

let execute_ping = _ => {
  Lwt.(
    Lwt_unix.sleep(1.0)
    >>= (
      () => {
        Console.log("sleep terminal");
        Lwt_result.return("ping");
      }
    )
  );
};

let execute_nothing = _ =>
  Lwt_result.return("Franchement, j'ai rien compris");

let get_direct_response_executor = (command, args) => {
  switch (command) {
  | "!ping" => DirectResponse.execute_ping()
  | _ => DirectResponse.execute_unknown()
  };
};

let execute = (action: Action.t) => {
  Lwt.(
    switch (action.runner) {
    | DirectResponse =>
      switch (action.on) {
      | Command(command, args) => get_direct_response_executor(command, args)
      | Unknown => DirectResponse.execute_unknown()
      }
    | HttpResponse(url, method, allowed, params, headers) =>
      HTTPResponse.execute_http_request(url, method, allowed, params, headers)
      >>= (body => Lwt_result.return(Response.Text(body)))
    }
  );
};
