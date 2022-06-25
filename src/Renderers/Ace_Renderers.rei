open Core.Types;

module ShellRenderer {
  let render: (Config.t, Interaction.t) => string
}
module SlackRenderer {
  let render: (Config.t, Interaction.t) => string
}
