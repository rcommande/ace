open Base;
open Cohttp;
open Cohttp_lwt_unix;
open Lwt;

type status_code =
  | S2xx
  | S3xx
  | S4xx
  | S5xx
  | S(int);

module Params = {
  type t = list((string, string));
};
module Headers = {
  type t = Cohttp.Header.t;
};

let get_status_code = resp => resp |> Response.status |> Code.code_of_status;

let is_allowed = (allowed, status) => {
  switch (allowed) {
  | S(code) => status == code
  | S2xx => status >= 200 && status < 300
  | S3xx => status >= 300 && status < 400
  | S4xx => status >= 400 && status < 500
  | S5xx => status >= 500 && status < 600
  };
};

let build_params = params =>
  List.fold(
    params,
    ~init="?",
    ~f=(acc, (name, value)) => {
      let param_str = name ++ "=" ++ value;
      Poly.(acc == "?") ? acc ++ param_str : acc ++ "&" ++ param_str;
    },
  );

let rec one = (items, ~f) => {
  switch (items) {
  | [] => false
  | [item, ...rest] => f(item) ? true : one(rest, f)
  };
};

let valid = (allowed, resp) => {
  let status = get_status_code(resp);
  one(allowed, code => is_allowed(code, status));
};

let handle_response = (allowed, (resp, body)) => {
  valid(allowed, resp)
    ? {
      Cohttp_lwt.Body.to_string(body)
      >>= (body_str => Lwt_result.return(body_str));
    }
    : {
      Lwt_result.fail("Status code error");
    };
};

let execute_http_request = (url, method, allowed, params, headers) => {
  let uri = Uri.of_string(url ++ build_params(params));
  Lwt.catch(
    () => Client.call(method, uri, ~headers) >>= handle_response(allowed),
    exn => {Lwt_result.fail("Unable to execute request")},
  );
};
