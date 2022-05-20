open Base;

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

let test = body => {
  let command = decode_slack_slash_command(body);

  Dream.debug(log => log("%s", command.text));
  Lwt.return();
};

let slack_slash = request => {
  let _ =
    Lwt.(
      Dream.body(request)
      >>= (
        body => {
          let _ = Lwt.async(() => test(body));
          Lwt.return();
        }
      )
    );
  Dream.(respond(~status=`OK, ""));
};

let start_server = () => {
  Dream.initialize_log(~level=`Debug, ());
  Dream.(
    run @@ logger @@ router([Dream.post("/slack/command", slack_slash)])
  );
};
