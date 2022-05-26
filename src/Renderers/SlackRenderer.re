open Base;
open Core;

let add_optional_field = (field, fields, ~f) => {
  switch (field) {
  | Some(field) => [f(field), ...fields]
  | None => fields
  };
};

let add_only_if = (field, fields, ~cond, ~f) => {
  cond() ? add_optional_field(field, fields, ~f) : fields;
};

module Slack = {
  type text_type =
    | PlainText
    | Mrkdwn;

  module CompositionObject = {
    module Text = {
      type t = {
        type_: text_type,
        text: string,
        verbatim: option(bool),
        emoji: option(bool),
      };

      let type_to_string = text_type =>
        switch (text_type) {
        | PlainText => "plain_text"
        | Mrkdwn => "mrkdwn"
        };

      let to_json = text => {
        let fields =
          [
            ("type", `String(type_to_string(text.type_))),
            ("text", `String(text.text)),
          ]
          |> add_only_if(
               text.emoji,
               ~cond=
                 () => {
                   switch (text.type_) {
                   | PlainText => true
                   | _ => false
                   }
                 },
               ~f=value => ("emoji", `Bool(value)),
             )
          |> add_optional_field(text.verbatim, ~f=value =>
               ("verbatim", `Bool(value))
             );
        `Assoc(fields);
      };
    };
    type t =
      /* | CompositionDialog */
      /* | Option_ */
      /* | OptionGroup */
      | Text(Text.t);

    let to_json = composition_object =>
      switch (composition_object) {
      | Text(text) => Text.to_json(text)
      };
  };

  module BlockElement = {
    module Button = {
      type t = {
        text: CompositionObject.Text.t,
        value: string,
        action_id: string,
      };

      let to_json = button => {
        let fields = [
          ("type", `String("button")),
          ("text", CompositionObject.Text.to_json(button.text)),
          ("value", `String(button.value)),
          ("action_id", `String(button.action_id)),
        ];
        `Assoc(fields);
      };
    };

    type t =
      /* | Checkboxes */
      /* | DatePicker */
      /* | Image */
      /* | MultiSelect */
      /* | PlainTextInput */
      /* | RadioButtons */
      /* | Select */
      /* | StaticSelect */
      /* | SuggestedActions */
      | Button(Button.t);

    let to_json = block_element => {
      switch (block_element) {
      | Button(button) => Button.to_json(button)
      };
    };
  };

  module Block = {
    module Section = {
      type t = {
        type_: text_type,
        text: string,
        emoji: option(bool),
        block_id: option(string),
        fields: option(list(CompositionObject.t)),
        accessory: option(BlockElement.t),
      };

      let to_json = (section: t) => {
        let text =
          CompositionObject.Text.{
            type_: section.type_,
            text: section.text,
            emoji: section.emoji,
            verbatim: None,
          };
        let fields =
          [
            ("type", `String("section")),
            ("text", text |> CompositionObject.Text.to_json),
          ]
          |> add_optional_field(section.block_id, ~f=value =>
               ("block_id", `String(value))
             )
          |> add_optional_field(
               section.fields,
               ~f=value => {
                 let texts =
                   List.map(value, ~f=text =>
                     CompositionObject.to_json(text)
                   );
                 ("fields", `List(texts));
               },
             )
          |> add_optional_field(section.accessory, ~f=value =>
               ("accessory", BlockElement.to_json(value))
             );
        `Assoc(fields);
      };
    };

    type t =
      /* | Action */
      /* | Context */
      /* | Divider */
      /* | File */
      /* | Header */
      /* | Image */
      /* | Input */
      | Section(Section.t);

    let to_json = block => {
      switch (block) {
      | Section(section) => Section.to_json(section)
      };
    };
  };

  module Attachment = {
    type color =
      | Good
      | Warning
      | Danger
      | Hex(string);

    type t = {
      blocks: list(Block.t),
      color,
    };

    let color_to_string = color =>
      switch (color) {
      | Good => "good"
      | Warning => "warning"
      | Danger => "danger"
      | Hex(hex) => hex
      };

    let to_json = attachment => {
      let fields = [
        (
          "blocks",
          `List(
            List.map(attachment.blocks, ~f=block => Block.to_json(block)),
          ),
        ),
        ("color", `String(color_to_string(attachment.color))),
      ];
      `Assoc(fields);
    };
  };

  module Response = {
    type t = {
      blocks: list(Block.t),
      attachments: option(list(Attachment.t)),
    };

    let to_json = response => {
      let blocks =
        List.map(response.blocks, ~f=block => Block.to_json(block));
      let fields =
        [("blocks", `List(blocks))]
        |> add_optional_field(response.attachments, ~f=value => {
             (
               "attachments",
               `List(
                 List.map(value, ~f=attachment =>
                   Attachment.to_json(attachment)
                 ),
               ),
             )
           });
      `Assoc(fields);
    };
  };

  let section =
      (~type_, ~text, ~emoji=?, ~block_id=?, ~fields=?, ~children=[], ()) => {
    let accessory =
      switch (children) {
      | [] => None
      | [first, ...rest] => Some(first)
      };
    let section =
      Block.Section.{type_, text, emoji, block_id, fields, accessory};
    Block.Section(section);
  };

  let button = (~type_, ~text, ~emoji=?, ~value, ~action_id, ~children=[], ()) => {
    let button =
      BlockElement.Button.{
        text: CompositionObject.Text.{type_, text, emoji, verbatim: None},
        value,
        action_id,
      };
    BlockElement.Button(button);
  };

  let group = (~children, ()) => children;
  let fields = group;
  let attachments = group;
  let blocks = group;

  let attachment = (~color, ~children=[], ()) => {
    Attachment.{blocks: children, color};
  };

  let message_response = (blocks, ~attachments=?, ()) => {
    let response = Response.{blocks, attachments};
    let json = Response.to_json(response);
    Yojson.Basic.to_string(json);
  };

  let text = (~type_, ~emoji=?, ~verbatim=?, ~children=[], ()) => {
    let text = String.concat(~sep="\n", children);
    let text_element =
      CompositionObject.Text.{type_, text, emoji: None, verbatim: None};
    CompositionObject.Text(text_element);
  };
};

let file_log_writer = r => {
  let _ =
    Lwt.(
      Lwt_io.(
        open_file(~mode=Output, "result.json")
        >>= (file => write_line(file, r))
      )
    );
  r;
};

let render_text = ((_, outgoing: Types.Outgoing.t)) => {
  let res = Types.Response.to_string(outgoing.response);
  Slack.(
    message_response(
      <blocks> <section type_=PlainText text=res /> </blocks>,
      ~attachments=
        <attachments>
          <attachment color={Attachment.Hex("#00bf29")}>
            <section type_=PlainText text=res />
            <section type_=PlainText text="Le blabla à côté du bouton">
              <button
                type_=PlainText
                text="cliquer ici"
                value="la valeur"
                action_id="id"
              />
            </section>
          </attachment>
        </attachments>,
      (),
    )
  );
};

let render_ok = (interaction: Types.Interaction.t) => {
  let (incoming, outgoing) = interaction;
  let res = Types.Response.to_string(outgoing.response);
  let command = incoming.input |> Types.Input.to_string;
  let txt = ":white_check_mark: " ++ res;
  let text_fields =
    Slack.(
      <fields>
        <text type_=Mrkdwn> "command" </text>
        <text type_=Mrkdwn> {"`" ++ command ++ "`"} </text>
      </fields>
    );
  let r =
    Slack.(
      message_response(
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
            <attachment color={Attachment.Hex("#00bf29")}>
              <section type_=Mrkdwn text="Result" emoji=true />
              <section type_=Mrkdwn text=txt emoji=true />
            </attachment>
          </attachments>,
        (),
      )
    );
  file_log_writer(r);
};

let render_error = render_text;
