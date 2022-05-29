open Base;

module Section = {
  type t = {
    type_: CompositionObject.Text.text_type,
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
      |> Utils.add_optional_field(section.block_id, ~f=value =>
           ("block_id", `String(value))
         )
      |> Utils.add_optional_field(
           section.fields,
           ~f=value => {
             let texts =
               List.map(value, ~f=text => CompositionObject.to_json(text));
             ("fields", `List(texts));
           },
         )
      |> Utils.add_optional_field(section.accessory, ~f=value =>
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
