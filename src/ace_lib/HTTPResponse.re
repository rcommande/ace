open Base;
open Cohttp_lwt_unix;
open Lwt;
open Yojson.Basic;
open Yojson;
open Types;

type action_result =
  | ResultOK(string, string)
  | ResultError(string);

module Headers = {
  type t = Cohttp.Header.t;
};

let get_status_code = resp =>
  resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status;

let is_allowed = (allowed, status) => {
  switch (allowed) {
  | Http.S(code) => status == code
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

let rec any = (items, ~f) => {
  switch (items) {
  | [] => false
  | [item, ...rest] => f(item) ? true : any(rest, ~f)
  };
};

let valid = (allowed, resp) => {
  let status = get_status_code(resp);
  any(allowed, ~f=code => is_allowed(code, status));
};

let decode_string = (json, fieldname) =>
  Util.member(fieldname, json) |> Util.to_string;

let decode_ok_response = json => {
  let content = decode_string(json, "content");
  let _type = decode_string(json, "type");
  ResultOK(content, _type);
};

let decode_error_response = json => {
  let message = decode_string(json, "message");
  ResultError(message);
};

let decode_result = json => {
  let status = decode_string(json, "status");
  switch (status) {
  | "ok" => decode_ok_response(json)
  | _ => decode_error_response(json)
  };
};

let build_response = res => {
  switch (res) {
  | ResultOK(content, _type) => Response.Text(Ok(content))
  | ResultError(message) => Response.Text(Error(message))
  };
};

let read_body = body => {
  open Response;
  let json = Basic.from_string(body);
  switch (decode_result(json)) {
  | item => build_response(item)
  | exception (Util.Type_error(_, _)) =>
    Response.Text(Error("Unreadable response"))
  };
};

let handle_response = (allowed, (resp, body)) => {
  valid(allowed, resp)
    ? {
      Cohttp_lwt.Body.to_string(body)
      >>= (body_str => Lwt_result.return(read_body(body_str)));
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
