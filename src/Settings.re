open Base;
open Lwt;
open Core;
open Yaml;

type slack = {oauth_token: string};

type t = {
  version: string,
  actions: array(Types.Action.t),
  default_action: Types.Action.t,
  slack: option(slack),
};

exception Config_error(string);
exception Validation_error(string);

module Decoder = {
  let string = (path, value: value) => {
    switch (value) {
    | `String(name) => name
    | _ => raise(Config_error(path ++ "must be a string"))
    };
  };

  let list = (decoder, path, yaml: value) => {
    switch (yaml) {
    | `A(values) => List.map(values, ~f=value => decoder(path, value))
    | _ => raise(Config_error(path ++ " must be a list"))
    };
  };

  let option = (decoder, path, yaml: value) => {
    switch (decoder(path, yaml)) {
    | value => Some(value)
    | exception _ => None
    };
  };

  let one_of = (path, choices, value) => {
    let choices_names =
      Array.is_empty(choices)
        ? ""
        : Array.reduce_exn(choices, ~f=(acc, choice) => {
            switch (acc) {
            | "" => choice
            | _ => acc ++ ", " ++ choice
            }
          });
    switch (Array.find(choices, ~f=choice => Poly.(choice == value))) {
    | Some(value) => value
    | None =>
      raise(
        Validation_error(path ++ " must be one of [" ++ choices_names ++ "]"),
      )
    };
  };

  let member = (~optional=false, key, yaml: value) => {
    switch (yaml) {
    | `O(values) =>
      let res =
        List.find(values, ~f=((k, value)) => Poly.(k == key) ? true : false);
      switch (res) {
      | Some((_, value)) => value
      | None =>
        optional
          ? `Null : raise(Config_error("Unable to find key : " ++ key))
      };
    | _ => raise(Config_error("Must be an object"))
    };
  };
};

let origin = (path, yaml) => {
  switch (yaml) {
  | `String(value) =>
    switch (Types.Service.from_string(value)) {
    | Ok(origin) => origin
    | Error(_) =>
      raise(Config_error(path ++ ": Possible values : \"shell\""))
    }
  | _ => raise(Config_error("Invalid origin"))
  };
};

let event = (path, yaml: value) => {
  switch (yaml) {
  | `O(events) =>
    open Decoder;
    let _type =
      yaml
      |> member("type")
      |> string(path ++ ".type")
      |> one_of(path ++ ".type", [|"command"|]);
    let name = yaml |> member("name") |> string(path ++ ".name");
    switch (_type) {
    | "command" => Types.Event.Command(name)
    | _ => raise(Config_error("Invalid event type : " ++ _type))
    };
  | _ => raise(Config_error(path ++ "Event must be an objet"))
  };
};

let params = (path, yaml: value) => {
  switch (yaml) {
  | `O(params) =>
    List.map(params, ~f=((key, value)) =>
      (key, Decoder.string(path ++ "." ++ key, value))
    )
  | _ => raise(Config_error(path ++ " must be an object"))
  };
};

let headers = (path, yaml: value) => {
  switch (yaml) {
  | `O(headers) =>
    List.map(headers, ~f=((key, value)) =>
      (key, Decoder.string(path ++ "." ++ key, value))
    )
    |> Cohttp.Header.of_list
  | _ => raise(Config_error(path ++ " must be an object"))
  };
};

let codes = (path, yaml: value) => {
  switch (yaml) {
  | `String(value) => Types.Http.status_code_from_string(value)
  | _ => raise(Config_error(path ++ " must be a string"))
  };
};

let http_response = (path, yaml: value) => {
  let url = Decoder.(yaml |> member("url") |> string(path ++ ".url"));
  let method =
    Decoder.(
      yaml
      |> member("method")
      |> string(path ++ ".")
      |> Cohttp.Code.method_of_string
    );
  let codes =
    Decoder.(yaml |> member("codes") |> list(codes, path ++ ".codes"));
  let params =
    Decoder.(yaml |> member("params") |> params(path ++ ".params"));
  let headers =
    Decoder.(yaml |> member("headers") |> headers(path ++ ".headers"));
  Types.Action.Runner.HttpResponse(url, method, codes, params, headers);
};

let runner = (path, yaml: value) => {
  switch (yaml) {
  | `O(runner) =>
    open Decoder;
    let _type = yaml |> member("type") |> string(path ++ ".type");
    switch (_type) {
    | "DirectResponse" => Types.Action.Runner.DirectResponse
    | "HttpResponse" => Decoder.(yaml |> http_response(path ++ ".runner"))
    | _ => raise(Config_error("Invalid Runner"))
    };
  | _ => raise(Config_error("Runner must be an object"))
  };
};

let action = (~on=[], path, yaml: value) => {
  switch (yaml) {
  | `O(_) =>
    let on_or_default =
      switch (yaml |> Decoder.member("on")) {
      | value => value |> Decoder.list(event, path ++ ".on")
      | exception _ => on
      };
    Decoder.(
      Types.Action.{
        name: yaml |> member("name") |> string(path ++ ".name"),
        from: yaml |> member("from") |> origin(path ++ ".from"),
        on: on_or_default,
        runner: yaml |> member("runner") |> runner(path ++ ".runner"),
      }
    );
  | _ => raise(Config_error("Invalid Action"))
  };
};

let slack = (path, yaml: value) => {
  switch (yaml) {
  | `O(_) => {
      oauth_token:
        Decoder.(
          yaml |> member("oauth_token") |> string(path ++ ".oauth_token")
        ),
    }
  | _ => raise(Config_error("Slack configuration must be an object"))
  };
};

let decode_config = content => {
  let yaml = Yaml.of_string_exn(content);
  Decoder.(
    Types.Action.{
      version: "0.1.0",
      actions:
        yaml |> member("actions") |> list(action, "") |> List.to_array,
      default_action:
        yaml |> member("default_action") |> action("default_action"),
      slack:
        yaml |> member("slack", ~optional=true) |> option(slack, "slack"),
    }
  );
};

let read_config_file = config_path => {
  let thread =
    Lwt_io.(open_file(~mode=Input, config_path))
    >>= (
      input_channel =>
        Lwt_io.read(input_channel)
        >>= (content => decode_config(content) |> Lwt_result.return)
    );
  Lwt.catch(
    () => thread,
    exn => {
      switch (exn) {
      | Config_error(msg)
      | Validation_error(msg) => Lwt_result.fail(msg)
      | _ => Lwt_result.fail("Unknown error")
      }
    },
  );
};

let read_config_file_sync = config_path => {
  switch (Stdio.In_channel.read_all(config_path) |> decode_config) {
  | config => Ok(config)
  | exception _ => Error("Unable to read config file")
  };
};
