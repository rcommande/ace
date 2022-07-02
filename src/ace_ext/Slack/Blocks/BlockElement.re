open Base;

module Button = {
  type t = {
    text: CompositionObject.Text.t,
    value: string,
    action_id: string,
  };

  let to_json = (button: t) => {
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
