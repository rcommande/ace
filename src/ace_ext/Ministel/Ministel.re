open Base;

/* type color = */
/*   | White */
/*   | Green */
/*   | Default */
/*   | Red; */

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default;

let to_ansiterminal_color = color => {
  switch (color) {
  | Black => ANSITerminal.black
  | Red => ANSITerminal.red
  | Green => ANSITerminal.green
  | Yellow => ANSITerminal.yellow
  | Blue => ANSITerminal.blue
  | Magenta => ANSITerminal.magenta
  | Cyan => ANSITerminal.cyan
  | White => ANSITerminal.white
  | Default => ANSITerminal.default
  };
};

module Terminal = {
  let createElement = (~color=Default, ~children=[], ()) => {
    let style = [color |> to_ansiterminal_color];
    let children_text = String.concat(~sep="", children);
    ANSITerminal.sprintf(style, "%s", children_text);
  };
};

module Line = {
  let createElement = (~children=[], ()) => {
    let children_text = String.concat(~sep="", children);
    children_text ++ "\r\n";
  };
};

module Text = {
  let createElement = (~color=Default, ~children=[], ()) => {
    let style = [color |> to_ansiterminal_color];
    let children_text = String.concat(~sep="", children);
    ANSITerminal.sprintf(style, "%s", children_text);
  };
};

module Br = {
  let createElement = (~children=[], ()) => {
    let _ = children;
    "\r\n";
  };
};
