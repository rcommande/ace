open Core.Types;

module type RendererImpl = {
  let render_text: Interaction.t => string;
  let render_error: Interaction.t => string;
  let render_ok: Interaction.t => string;
};

module Renderer = (Implementation: RendererImpl) => {
  let render = (interaction: Interaction.t) => {
    let (_, outgoing) = interaction;
    let response = outgoing.response;
    switch (response) {
    | Response.Text(text_res) =>
      switch (text_res) {
      | Ok(text) => Implementation.render_text(interaction)
      | Error(text) => Implementation.render_error(interaction)
      }
    | Response.Error(_) => Implementation.render_error(interaction)
    | Response.Ok_ => Implementation.render_ok(interaction)
    };
  };
};

module ShellRenderer = Renderer(ShellRenderer);
