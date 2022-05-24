open Base;

module Http = {
  type status_code =
    | S2xx
    | S3xx
    | S4xx
    | S5xx
    | S(int);

  let status_code_from_string = code_str => {
    switch (code_str) {
    | "S2xx" => S2xx
    | "S3xx" => S3xx
    | "S4xx" => S4xx
    | "S5xx" => S5xx
    | _ =>
      switch (Int.of_string(code_str)) {
      | code => S(code)
      | exception _ => raise(Invalid_argument(code_str))
      }
    };
  };

  type params = list((string, string));
};

module Response = {
  type t =
    | Text(Result.t(string, string))
    | Ok_
    | Error(string);

  let to_string = response => {
    switch (response) {
    | Text(text_res) =>
      switch (text_res) {
      | Ok(text) => text
      | Error(err) => err
      }
    | Ok_ => "OK"
    | Error(msg) => "error: " ++ msg
    };
  };
};

module Service = {
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

  let from_string = value => {
    switch (value) {
    | "all" => Ok(All)
    | "slack" => Ok(Slack)
    | "event" => Ok(Event)
    | "shell" => Ok(Shell)
    | _ => Error("Invalid origin")
    };
  };
};

module Input = {
  type t =
    | Command(string, string, list(string))
    | Sentence(string);

  let to_string = input =>
    switch (input) {
    | Command(input, _, _) => input
    | Sentence(sentence) => sentence
    };
};

module Event = {
  type t =
    | Command(string);

  let to_string = event_opt => {
    switch (event_opt) {
    | Some(Command(_)) => "command"
    | None => ""
    };
  };
};

module Action = {
  module Runner = {
    type url = string;

    type t =
      | HttpResponse(
          url,
          Cohttp.Code.meth,
          list(Http.status_code),
          Http.params,
          Cohttp.Header.t,
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
    from: Service.t,
    on: list(Event.t),
    runner: Runner.t,
  };

  let to_string = action => action.name;
};

module Incoming = {
  type t = {
    input: Input.t,
    origin: Service.t,
    destination: Service.t,
  };
};

module Outgoing = {
  type t = {
    action: Action.t,
    response: Response.t,
    event: option(Event.t),
  };
};

module Interaction = {
  type t = (Incoming.t, Outgoing.t);

  let make = (incoming, outgoing) => {
    (incoming, outgoing);
  };
};
