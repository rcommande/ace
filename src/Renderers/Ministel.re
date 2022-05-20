type color =
  | White
  | Green
  | Red;

let createElement = (~color=ANSITerminal.white, ~children=[], ()) => {
  let style = [color];
  let children_text = String.concat("", children);
  ANSITerminal.sprintf(style, "%s", children_text);
};
