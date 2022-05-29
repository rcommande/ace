open Base;

module Text = {
  type text_type =
    | PlainText
    | Mrkdwn;

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
      |> Utils.add_only_if(
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
      |> Utils.add_optional_field(text.verbatim, ~f=value =>
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
