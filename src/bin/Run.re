open Ace;
open Ace.Core;
open Base;
open Cohttp;
open Cohttp_lwt_unix;

/* To delete */
let default: Types.Action.t = {
  name: "Unknown",
  from: Types.Origin.Shell,
  on: [],
  runner: Types.Action.Runner.DirectResponse,
};
/* end to delete */

type slack_slash_command = {
  token: string,
  team_id: string,
  team_domain: string,
  channel_id: string,
  channel_name: string,
  user_id: string,
  user_name: string,
  command: string,
  text: string,
  response_url: string,
};

let find = (form, key) => {
  let (_, value) = List.find_exn(form, ~f=((k, v)) => Poly.(k == key));
  value;
};

let decode_slack_slash_command = body => {
  let form = Dream.from_form_urlencoded(body);
  {
    token: find(form, "token"),
    team_id: find(form, "team_id"),
    team_domain: find(form, "team_domain"),
    channel_id: find(form, "channel_id"),
    channel_name: find(form, "channel_name"),
    user_id: find(form, "user_id"),
    user_name: find(form, "user_name"),
    command: find(form, "command"),
    text: find(form, "text"),
    response_url: find(form, "response_url"),
  };
};

let get_command_thread = (text, config: Ace.Settings.t) => {
  switch (String.split(text, ~on=' ')) {
  | ["ping", ...rest] =>
    let input_res = Processor.process_input("!ping");
    switch (input_res) {
    | Ok(input) =>
      let incoming =
        Types.Incoming.{
          input,
          origin: Types.Origin.Slack,
          destination: Types.Origin.Slack,
        };
      let (action, event_opt) =
        Processor.find_action_or_default(incoming, config.actions, default);
      Some(Processor.execute(incoming, event_opt, action));
    | Error(message) => None
    };
  | _ => None
  };
};

let test = (body, config) => {
  let command = decode_slack_slash_command(body);

  let thread_opt = get_command_thread(command.text, config);
  let _ =
    Lwt.(
      switch (thread_opt) {
      | Some(thread) =>
        thread
        >>= (
          incoming_res => {
            switch (incoming_res) {
            | Ok(interaction) =>
              let body =
                interaction
                |> Ace_Renderers.SlackRenderer.render
                |> Cohttp_lwt.Body.of_string;
              let uri = Uri.of_string(command.response_url);
              Client.post(~body, uri) >>= (_ => Lwt.return());
            | Error(msg) => Lwt_io.(write_line(stdout, msg))
            };
          }
        )
      | None => Lwt_io.(write_line(stdout, "Error !"))
      }
    );
  Lwt.return();
};

let slack_slash = (config, request) => {
  let _ =
    Lwt.(
      Dream.body(request)
      >>= (
        body => {
          let _ = Lwt.async(() => test(body, config));
          Lwt.return();
        }
      )
    );
  Dream.(respond(~status=`OK, ""));
};

let start_server = () => {
  let config_path = "config.yaml";
  let config =
    Lwt.(
      Lwt_main.run(
        Settings.read_config_file(config_path)
        >>= (
          config_res => {
            switch (config_res) {
            | Ok(config) => Lwt.return(config)
            | Error(_) => Lwt.fail(Not_found)
            };
          }
        ),
      )
    );
  Dream.initialize_log(~level=`Debug, ());
  Dream.(
    run @@
    logger @@
    router([Dream.post("/slack/command", slack_slash(config))])
  );
  ();
};
