open Core.Types;
/* open Pastel; */

module InteractionItem = {
  let createElement = (~key, ~value, ~color, ~children=[], ()) => {
    /* <Pastel> */
    /*   <Pastel color> {key ++ ": "} </Pastel> */
    /*   <Pastel color=Pastel.White> value </Pastel> */
    "Ok";
    /* </Pastel>; */
  };
};

let render_interaction = (interaction: Interaction.t, color) => {
  /* let (incoming, outgoing) = interaction; */
  /* <Pastel> */
  /*   <InteractionItem */
  /*     key="INPUT" */
  /*     value={"\"" ++ Input.to_string(incoming.input) ++ "\""} */
  /*     color */
  /*   /> */
  /*   "\r\n" */
  /*   <InteractionItem */
  /*     key="ORIGIN" */
  /*     value={Origin.to_string(incoming.origin)} */
  /*     color */
  /*   /> */
  /*   "\r\n" */
  /*   <InteractionItem */
  /*     key="DESTINATION" */
  /*     value={Origin.to_string(incoming.destination)} */
  /*     color */
  /*   /> */
  /*   "\r\n" */
  /*   <InteractionItem */
  /*     key="ACTION" */
  /*     value={Action.to_string(outgoing.action)} */
  /*     color */
  /*   /> */
  /*   "\r\n" */
  /*   <InteractionItem */
  /*     key="RUNNER" */
  /*     value={Action.Runner.to_string(outgoing.action.runner)} */
  /*     color */
  /*   /> */
  /*   "\r\n" */
  /*   <InteractionItem */
  /*     key="RESPONSE" */
  /*     value={Response.to_string(outgoing.response)} */
  /*     color */
  /*   /> */
  /*   "\r\n" */
  /*   <InteractionItem */
  /*     key="EVENT" */
  /*     value={Event.to_string(outgoing.event)} */
  /*     color */
  /*   /> */
  /*   "\r\n" */
  "ok";
  /* </Pastel>; */
};

let render_text = interaction =>
  render_interaction(interaction, "Pastel.Green");
let render_error = interaction => {
  render_interaction(interaction, "Pastel.Red");
};
let render_ok = interaction =>
  render_interaction(interaction, "Pastel.Green");
