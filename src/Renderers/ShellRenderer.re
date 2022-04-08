open Core.Types;
/* open Pastel; */
open ANSITerminal;

module InteractionItem = {
  let createElement = (~key, ~value, ~color, ~children=[], ()) => {
    <Ministel>
      <Ministel color> {key ++ ": "} </Ministel>
      <Ministel color=white> value </Ministel>
    </Ministel>;
  };
};

let render_interaction = (interaction: Interaction.t, color) => {
  let (incoming, outgoing) = interaction;
  <Ministel>
    <InteractionItem
      key="INPUT"
      value={"\"" ++ Input.to_string(incoming.input) ++ "\""}
      color
    />
    "\r\n"
    <InteractionItem
      key="ORIGIN"
      value={Origin.to_string(incoming.origin)}
      color
    />
    "\r\n"
    <InteractionItem
      key="DESTINATION"
      value={Origin.to_string(incoming.destination)}
      color
    />
    "\r\n"
    <InteractionItem
      key="ACTION"
      value={Action.to_string(outgoing.action)}
      color
    />
    "\r\n"
    <InteractionItem
      key="RUNNER"
      value={Action.Runner.to_string(outgoing.action.runner)}
      color
    />
    "\r\n"
    <InteractionItem
      key="RESPONSE"
      value={Response.to_string(outgoing.response)}
      color
    />
    "\r\n"
    <InteractionItem
      key="EVENT"
      value={Event.to_string(outgoing.event)}
      color
    />
    "\r\n"
  </Ministel>;
};

let render_text = interaction => render_interaction(interaction, green);
let render_error = interaction => render_interaction(interaction, red);
let render_ok = interaction => render_interaction(interaction, green);
