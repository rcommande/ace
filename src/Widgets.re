open Pastel;
open Processor;

type t =
  | TextWidget(string, result(string, string), Action.t)
  | ErrorWidget(string, string, Action.t);

module Destination = {
  type t =
    | Shell;

  let to_string = destination => {
    switch (destination) {
    | Shell => "Shell"
    };
  };
};

module TextWidget = {
  let item = (~valid=true, ~name, ~value, ~children=[], ()) => {
    let color = valid ? Green : Red;
    <Pastel>
      <Pastel color> {name ++ ": "} </Pastel>
      <Pastel color=White> value </Pastel>
    </Pastel>
    ++ "\n";
  };

  let render_interation =
      (valid, input, response, action: Action.t, destination) =>
    <Pastel>
      <item valid name="INPUT" value=input />
      <item valid name="INTENT" value={action.name} />
      <item
        valid
        name="EVENT TYPE"
        value={action.on |> Action.Event.to_string}
      />
      <item valid name="FROM" value={action.from |> Origin.to_string} />
      <item
        valid
        name="DESTINATION"
        value={destination |> Destination.to_string}
      />
      <item
        valid
        name="RUNNER"
        value={action.runner |> Action.Runner.to_string}
      />
      <item valid name="RESPONSE" value=response />
    </Pastel>;

  let render = (input, response, dest: Destination.t, action: Action.t) => {
    switch (response) {
    | Ok(text) => render_interation(true, input, text, action, dest)
    | Error(text) => render_interation(false, input, text, action, dest)
    };
  };
};

module ErrorWidget = {
  let render = (input, error_text, dest: Destination.t, action) => {
    switch (dest) {
    | Shell => <Pastel color=Red> error_text </Pastel>
    };
  };
};

let render = (destination: Destination.t, widget: t) => {
  switch (widget) {
  | TextWidget(input, response, action) =>
    TextWidget.render(input, response, destination, action)
  | ErrorWidget(input, text, action) =>
    ErrorWidget.render(input, text, destination, action)
  };
};

let make =
    (
      input: string,
      action: Processor.Action.t,
      response: result(Response.t, string),
    ) => {
  switch (response) {
  | Ok(Text(value)) => TextWidget(input, value, action)
  | Ok(Error(text))
  | Error(text) => ErrorWidget(input, text, action)
  };
};
