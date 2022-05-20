let run = () =>
  Dream.(run @@ logger @@ router([Dream.get("/", _ => html("world"))]));
