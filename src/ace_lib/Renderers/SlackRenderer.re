open Base;
open Externals.Slack.Blocks;

let render_ok = (config: Types.Config.t, interaction: Types.Interaction.t) => {
  let (incoming, outgoing) = interaction;
  let res = Types.Response.to_string(outgoing.response);
  let command = incoming.input |> Types.Input.to_string;
  let txt = ":white_check_mark: " ++ res;
  let text_fields =
    <fields>
      <text type_=Mrkdwn> "command" </text>
      <text type_=Mrkdwn> {"`" ++ command ++ "`"} </text>
    </fields>;
  response(
    <blocks>
      <section
        type_=PlainText
        emoji=true
        text=":gear: Command received"
        fields=text_fields
      />
    </blocks>,
    ~attachments=
      <attachments>
        <attachment color={Hex("#00bf29")}>
          <section type_=Mrkdwn text="Result" emoji=true />
          <section type_=Mrkdwn text=txt emoji=true />
        </attachment>
      </attachments>,
  );
};

let render_error = (_, _) => "";
let render_text = (_, _) => "";
