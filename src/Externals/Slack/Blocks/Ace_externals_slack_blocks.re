/* module Response = Response; */
/* module LayoutBlock = LayoutBlock; */
/* module CompositionObject = CompositionObject; */
/* module BlockElement = BlockElement; */

/* response = Jsx.response; */
/* text = Jsx.Text; */
/* attachment */
open Base;

let section =
    (~type_, ~text, ~emoji=?, ~block_id=?, ~fields=?, ~children=[], ()) => {
  let accessory =
    switch (children) {
    | [] => None
    | [first, ...rest] => Some(first)
    };
  let section =
    LayoutBlock.Section.{type_, text, emoji, block_id, fields, accessory};
  LayoutBlock.Section(section);
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

let response = (~attachments=?, blocks) => {
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

type text_type = CompositionObject.Text.text_type;
type color = Attachment.color;
