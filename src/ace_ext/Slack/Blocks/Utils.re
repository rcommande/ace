let add_optional_field = (field, fields, ~f) => {
  switch (field) {
  | Some(field) => [f(field), ...fields]
  | None => fields
  };
};

let add_only_if = (field, fields, ~cond, ~f) => {
  cond() ? add_optional_field(field, fields, ~f) : fields;
};
