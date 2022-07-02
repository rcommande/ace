open Types;
open ANSITerminal;
open Externals;

module InteractionItem = {
  let createElement = (~key, ~value, ~color, ~children=[], ()) => {
    Ministel.(
      <Terminal>
        <Text color> {key ++ ": "} </Text>
        <Text color=White> value </Text>
      </Terminal>
    );
  };
};

module ResponseItem = {
  let createElement = (~botname, ~value, ~color, ~children=[], ()) => {
    Ministel.(
      <Terminal>
        <Line> <Text color=Blue> {botname ++ " > " ++ value} </Text> </Line>
      </Terminal>
    );
  };
};

let render_interaction = (config: Config.t, interaction: Interaction.t, color) => {
  let (incoming, outgoing) = interaction;

  Ministel.(
    <Terminal>
      <Line>
        <InteractionItem
          key="INPUT"
          value={"\"" ++ Input.to_string(incoming.input) ++ "\""}
          color
        />
      </Line>
      <Line>
        <InteractionItem
          key="ORIGIN"
          value={Service.to_string(incoming.origin)}
          color
        />
      </Line>
      <Line>
        <InteractionItem
          key="DESTINATION"
          value={Service.to_string(incoming.destination)}
          color
        />
      </Line>
      <Line>
        <InteractionItem
          key="ACTION"
          value={Action.to_string(outgoing.action)}
          color
        />
      </Line>
      <Line>
        <InteractionItem
          key="RUNNER"
          value={Action.Runner.to_string(outgoing.action.runner)}
          color
        />
      </Line>
      <Line>
        <InteractionItem
          key="EVENT"
          value={Event.to_string(outgoing.event)}
          color
        />
      </Line>
      <Br />
      <Line>
        <ResponseItem
          botname={config.bot.name}
          value={Response.to_string(outgoing.response)}
          color
        />
      </Line>
    </Terminal>
  );
};

let render_text = (config, interaction) =>
  render_interaction(config, interaction, Ministel.Green);

let render_error = (config, interaction) =>
  render_interaction(config, interaction, Ministel.Red);

let render_ok = (config, interaction) =>
  render_interaction(config, interaction, Ministel.Green);
