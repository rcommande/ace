open Base;

type t = {
  blocks: list(LayoutBlock.t),
  attachments: option(list(Attachment.t)),
};

let to_json = (response: t) => {
  let blocks = List.map(response.blocks, ~f=block => LayoutBlock.to_json(block));
  let fields =
    [("blocks", `List(blocks))]
    |> Utils.add_optional_field(response.attachments, ~f=value => {
         (
           "attachments",
           `List(
             List.map(value, ~f=attachment => Attachment.to_json(attachment)),
           ),
         )
       });
  `Assoc(fields);
};
