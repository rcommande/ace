open Base;

type color =
  | White
  | Green
  | Red;

module Terminal = {
  let createElement = (~color=ANSITerminal.default, ~children=[], ()) => {
    let style = [color];
    let children_text = String.concat(~sep="", children);
    ANSITerminal.sprintf(style, "%s", children_text);
  };
};

module Line = {
  let createElement = (~color=ANSITerminal.default, ~children=[], ()) => {
    let children_text = String.concat(~sep="", children);
    children_text ++ "\r\n";
  };
};

module Text = {
  let createElement = (~color=ANSITerminal.default, ~children=[], ()) => {
    let style = [color];
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
