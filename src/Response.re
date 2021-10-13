open Base;

type t =
  | Text(Result.t(string, string))
  | Error(string);
