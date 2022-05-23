open Core;

let render_text = ((_, outgoing: Types.Outgoing.t)) => {
  let response = Types.Response.to_string(outgoing.response);
  "{\"text\": \"" ++ response ++ "\"}";
};

let render_error = render_text;
let render_ok = render_text;
