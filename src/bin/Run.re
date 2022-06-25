open Ace;
open Ace.Core;
open Base;
open Cohttp;
open Cohttp_lwt_unix;

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

type errors =
  | ResponseSendingError(exn)
  | CommandExecutionError(string);

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

let send_response = (content, response_url) => {
  let uri = Uri.of_string(response_url);
  Lwt.(
    catch(
      () =>
        Client.post(~body=content |> Cohttp_lwt.Body.of_string, uri)
        >>= (_ => Lwt_result.return()),
      exn => Lwt_result.fail(ResponseSendingError(exn)),
    )
  );
};

let execute_command = (body, config: Types.Config.t) => {
  let slack_command = decode_slack_slash_command(body);
  Dream.info(log => log("Slack command received: %s", slack_command.text));
  Lwt.(
    Processor.run_command(
      "!" ++ slack_command.text,
      config.actions,
      config.default_action,
      Types.Service.Slack,
      Types.Service.Slack,
    )
    >>= (
      interaction_res => {
        switch (interaction_res) {
        | Ok(interaction) =>
          let body = Ace_Renderers.SlackRenderer.render(config, interaction);
          let _ = send_response(body, slack_command.response_url);
          Lwt_result.return(body);
        | Error(msg) => Lwt_result.fail(CommandExecutionError(msg))
        };
      }
    )
  );
};

let log_result = res => {
  let log = Dream.sub_log("ace.run");
  switch (res) {
  | Ok(body) =>
    log.info(log => log("%s", "Slack command response sent successfully"));
    log.debug(log => log("%s", body));
  | Error(ResponseSendingError(_)) =>
    log.error(log => log("%s", "Error while sending response to Slack"))
  | Error(CommandExecutionError(msg)) => log.error(log => log("%s", msg))
  };
  Lwt.return();
};

let slack_slash = (config, request) => {
  let _ =
    Lwt.(
      Dream.body(request)
      >>= (
        body => {
          async(() => {execute_command(body, config) >>= log_result});
          Lwt.return();
        }
      )
    );
  Dream.(respond(~status=`OK, ""));
};

let start_server = () => {
  let config_res = ConfigParser.read_config_file_sync("config.yaml");
  switch (config_res) {
  | Ok(config) =>
    Dream.set_log_level("ace.run", `Debug);
    let _ =
      Dream.(
        run @@
        logger @@
        router([Dream.post("/slack/command", slack_slash(config))])
      );
    ();
  | Error(msg) => Stdio.Out_channel.(output_string(stdout, msg))
  };
  Stdio.Out_channel.(output_char(stdout, '\n'));
  Stdio.Out_channel.(flush(stdout));
};
