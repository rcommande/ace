open React;
open Lwt;
open Base;
open Stdio;
open Ace;
open Processor;

let actions =
  Action.[
    {
      name: "Ping response",
      from: Origin.Shell,
      on: Action.Event.Command("!ping", None),
      runner: Action.Runner.DirectResponse,
    },
    {
      name: "test http response",
      from: Origin.Shell,
      on: Action.Event.Command("!hello", None),
      runner:
        Action.Runner.HttpResponse(
          "http://localhost:5000/",
          Cohttp.Code.method_of_string("GET"),
          [HTTPResponse.S2xx],
          [("toto", "titi"), ("bidule", "machin")],
          Cohttp.Header.of_list([
            ("Content-type", "text/plain"),
            ("machine", "trucmuche"),
          ]),
        ),
    },
  ];

let default =
  Action.{
    name: "Unknown",
    from: Processor.Origin.Shell,
    on: Processor.Action.Event.Unknown,
    runner: Processor.Action.Runner.DirectResponse,
  };

let shell_commands = ["exit", "quit", "clear"];

let handle_result = (input, action, response) => {
  let output =
    Widgets.make(input, action, response)
    |> Widgets.render(Widgets.Destination.Shell);
  Lwt_io.write_line(Lwt_io.stdout, output);
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

  let string = text => [LTerm_text.S(text)];

  let style =
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
    let c =
      List.fold(
        children,
        ~f=(acc, child) => List.append(acc, child),
        ~init=[],
      );
    c
    |> fg(foreground)
    |> bg(background)
    |> b(Some(bold))
    |> u(Some(underline))
    |> bk(Some(blink))
    |> r(Some(reverse));
  };

  let text = (~children=[], ()) => {
    List.map(children, ~f=child => LTerm_text.S(child));
  };
};

let make_prompt = line_number => {
  let formated_line_number = line_number |> Int.to_string;
  let markup =
    Prompt.(
      <style foreground=LTerm_style.green>
        <text> "In [" </text>
        <style foreground=LTerm_style.lgreen bold=true>
          <text> formated_line_number </text>
        </style>
        <text> "]: " </text>
      </style>
    );
  markup |> LTerm_text.eval;
};

let time = {
  let (time, set_time) = S.create(Unix.time());
  /* Update the time every second. */
  ignore(Lwt_engine.on_timer(1.0, true, _ => set_time(Unix.time())));
  time;
};

let execute_command = input => {
  let action =
    Processor.process_input(Processor.Origin.Shell, input, actions, default);
  Processor.execute(action)
  >>= (response => handle_result(input, action, response));
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
    this#set_prompt(
      S.l2((size, time) => make_prompt(line_number), this#size, time),
    )
  );
};

let get_binaries = (actions: list(Action.t), shell_commands) => {
  let commands =
    List.filter(actions, ~f=action => {
      switch (action.on) {
      | Action.Event.Command(_, _) => true
      | _ => false
      }
    });
  let command_list =
    List.map(
      commands,
      ~f=action => {
        let command_name =
          switch (action.on) {
          | Action.Event.Command(name, _) => name
          | _ => ""
          };
        Zed_string.unsafe_of_utf8(command_name);
      },
    );
  List.concat([
    command_list,
    List.map(shell_commands, ~f=shell_command =>
      Zed_string.unsafe_of_utf8(shell_command)
    ),
  ]);
};

let rec loop = (~binaries, ~term, ~history, ~exit_code, ~line_number=1, ()) => {
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
            let command = ("clear", [|"clear"|]);
            Lwt_process.exec(command) >>= (exit_code => Lwt_result.return());
          | _ => execute_command(input) >>= (_ => Lwt_result.return())
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
              (),
            )
        );
      };
    }
  );
};

let run_shell = () => {
  let binaries = get_binaries(actions, shell_commands);
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
            (),
          )
      )
  );
};
