open Base;

type color =
  | Good
  | Warning
  | Danger
  | Hex(string);

type t = {
  blocks: list(LayoutBlock.t),
  color,
};

let color_to_string = color =>
  switch (color) {
  | Good => "good"
  | Warning => "warning"
  | Danger => "danger"
  | Hex(hex) => hex
  };

let to_json = (attachment: t) => {
  let fields = [
    (
      "blocks",
      `List(
        List.map(attachment.blocks, ~f=block => LayoutBlock.to_json(block)),
      ),
    ),
    ("color", `String(color_to_string(attachment.color))),
  ];
  `Assoc(fields);
};
