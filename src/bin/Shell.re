open Ace;
open Ace_Renderers;
open Base;
open Cohttp;
open Core.Types.Action;
open Core.Types.Http;
open Core.Types.Service;
open Core.Types;
open Core;
open Lwt;
open Processor;
open React;
open Stdio;

let shell_commands = [|"exit", "quit", "clear"|];

let handle_result = (config, interaction_res) => {
  let io_writer = Lwt_io.write_line(Lwt_io.stdout);
  switch (interaction_res) {
  | Ok(interaction) => io_writer @@ ShellRenderer.render(config, interaction)
  | Error(error) => io_writer(error)
  };
};

module Prompt = {
  let wrap_markup = (value, markup, _start, _end) => {
    LTerm_text.(
      switch (value) {
      | None => markup
      | Some(value) =>
        List.append(List.append([_start(value)], markup), [_end])
      }
    );
  };

  let fg = (color, markup) =>
    wrap_markup(
      color,
      markup,
      color => LTerm_text.B_fg(color),
      LTerm_text.E_fg,
    );
  let bg = (color, markup) =>
    wrap_markup(
      color,
      markup,
      color => LTerm_text.B_bg(color),
      LTerm_text.E_bg,
    );
  let b = (value, markup) =>
    wrap_markup(
      value,
      markup,
      value => LTerm_text.B_bold(value),
      LTerm_text.E_bold,
    );
  let u = (value, markup) =>
    wrap_markup(
      value,
      markup,
      value => LTerm_text.B_underline(value),
      LTerm_text.E_underline,
    );
  let bk = (value, markup) =>
    wrap_markup(
      value,
      markup,
      value => LTerm_text.B_blink(value),
      LTerm_text.E_blink,
    );
  let r = (value, markup) =>
    wrap_markup(
      value,
      markup,
      value => LTerm_text.B_reverse(value),
      LTerm_text.E_reverse,
    );

  module Text = {
    let createElement = (~children=[], ()) => {
      List.map(children, ~f=child => LTerm_text.S(child));
    };
  };

  module Style = {
    let createElement =
        (
          ~foreground=?,
          ~background=?,
          ~bold=false,
          ~underline=false,
          ~blink=false,
          ~reverse=false,
          ~children=[],
          (),
        ) => {
      List.fold(
        children,
        ~f=(acc, child) => List.append(acc, child),
        ~init=[],
      )
      |> fg(foreground)
      |> bg(background)
      |> b(Some(bold))
      |> u(Some(underline))
      |> bk(Some(blink))
      |> r(Some(reverse));
    };
  };
};

let make_prompt = line_number => {
  let formated_line_number = line_number |> Int.to_string;
  let markup =
    Prompt.(
      <Style foreground=LTerm_style.green>
        <Style foreground=LTerm_style.yellow>
          <Text> {"[" ++ formated_line_number ++ "]"} </Text>
        </Style>
        <Text> " you > " </Text>
      </Style>
    );
  markup |> LTerm_text.eval;
};

let execute_command = (input_text, config: Config.t) => {
  let input_res = Processor.process_input(input_text);
  switch (input_res) {
  | Ok(input) =>
    let incoming =
      Incoming.{input, origin: Service.Shell, destination: Service.Shell};
    let (action, event_opt) =
      Processor.find_action_or_default(
        incoming,
        config.actions,
        config.default_action,
      );
    action
    |> Processor.execute(incoming, event_opt)
    >>= handle_result(config);
  | Error(message) => Lwt.return()
  };
};

class read_line (~term, ~history, ~exit_code, ~binaries, ~line_number) = {
  inherit (class LTerm_read_line.read_line)(~history, ());
  inherit (class LTerm_read_line.term(Zed_string.t))(term);
  pub! completion = {
    let prefix = Zed_rope.to_string(this#input_prev);
    let binaries =
      List.filter(binaries, ~f=binary => {
        Zed_string.starts_with(binary, ~prefix)
      });

    this#set_completion(
      0,
      List.map(binaries, ~f=binary =>
        (binary, Zed_string.unsafe_of_utf8(""))
      ),
    );
  };
  initializer (
    this#set_prompt(S.l1(size => make_prompt(line_number), this#size))
  );
};

let get_binaries = (config: Config.t, shell_commands) => {
  List.concat([
    config.actions
    |> Array.to_list
    |> List.map(~f=(action: Action.t) => {
         List.filter(action.on, ~f=event =>
           switch (event) {
           | Event.Command(_) => true
           }
         )
         |> List.map(~f=event =>
              switch (event) {
              | Event.Command(command) =>
                Zed_string.unsafe_of_utf8("!" ++ command)
              }
            )
       })
    |> List.concat,
    Array.map(shell_commands, ~f=command =>
      Zed_string.unsafe_of_utf8(command)
    )
    |> Array.to_list,
  ]);
};

let rec loop =
        (~binaries, ~term, ~history, ~exit_code, ~line_number=1, ~config, ()) => {
  let read_line_engine =
    (new read_line)(
      ~term,
      ~history=LTerm_history.contents(history),
      ~exit_code,
      ~binaries,
      ~line_number,
    );

  read_line_engine#run
  >>= (
    result => {
      LTerm_history.add(history, result);
      let input = result |> Zed_string.to_utf8;
      switch (input) {
      | "quit"
      | "exit" => Lwt_result.return()
      | _ =>
        let execution =
          switch (input) {
          | "clear" =>
            let command = Lwt_process.shell("clear");
            Lwt_process.exec(command) >>= (_ => Lwt_result.return());
          | _ => execute_command(input, config) >>= (_ => Lwt_result.return())
          };
        execution
        >>= (
          _ =>
            loop(
              ~binaries,
              ~term,
              ~history,
              ~exit_code,
              ~line_number=line_number + 1,
              ~config,
              (),
            )
        );
      };
    }
  );
};

let run_shell = config => {
  let binaries = get_binaries(config, shell_commands);
  LTerm_inputrc.load()
  >>= (
    () =>
      Lazy.force(LTerm.stdout)
      >>= (
        term =>
          loop(
            ~binaries,
            ~term,
            ~history=LTerm_history.create([]),
            ~exit_code=0,
            ~config,
            (),
          )
      )
  );
};

let run = () => {
  let _ =
    switch (ConfigParser.read_config_file_sync("config.yaml")) {
    | Ok(config) =>
      let _ = run_shell(config) |> Lwt_main.run;
      ();
    | Error(msg) =>
      let _ =
        Lwt_io.(
          write_line(stdout, "Unable to read the config file : " ++ msg)
        );
      ();
    };
  ();
};
