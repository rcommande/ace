open Ace.Processor.Action;
open Ace.Processor;
open Ace;
open Base;
open TestFramework;

let ping_action = {
  name: "Ping action",
  from: Origin.All,
  on: Event.Command("!ping", None),
  runner: Runner.DirectResponse,
};

let actions = [
  ping_action,
  {
    name: "Specific shell action",
    from: Origin.Shell,
    on: Event.Command("!shell", None),
    runner: Runner.DirectResponse,
  },
];

let default_action = {
  name: "Unknown action (default action)",
  from: Origin.All,
  on: Action.Event.Unknown,
  runner: Action.Runner.DirectResponse,
};

describe("process_input", ({test}) => {
  open Processor.Action;

  test("test process a valid ping command", ({expect}) => {
    let origin = Processor.Origin.Shell;
    let input = "!ping";
    let expected = {
      name: "Ping action",
      from: Origin.All,
      on: Event.Command("!ping", None),
      runner: Runner.DirectResponse,
    };

    let res = Processor.process_input(origin, input, actions, default_action);

    expect.equal(expected, res);
  });

  test("test process a valid ping command with arguments", ({expect}) => {
    let origin = Processor.Origin.Shell;
    let input = "!ping one two three";
    let expected = {
      name: "Ping action",
      from: Origin.All,
      on: Event.Command("!ping", None),
      runner: Runner.DirectResponse,
    };

    let res = Processor.process_input(origin, input, actions, default_action);

    expect.equal(expected, res);
  });

  test("test process a unknown command", ({expect}) => {
    let origin = Processor.Origin.Shell;
    let input = "!pong";
    let expected = default_action;

    let res = Processor.process_input(origin, input, actions, default_action);

    expect.equal(expected, res);
  });

  test("test process an invalid command", ({expect}) => {
    let origin = Processor.Origin.Shell;
    let input = "!!!!!";
    let expected = default_action;

    let res = Processor.process_input(origin, input, actions, default_action);

    expect.equal(expected, res);
  });

  test("test process an invalid command with arguments", ({expect}) => {
    let origin = Processor.Origin.Shell;
    let input = "!!!!!one two three";
    let expected = default_action;

    let res = Processor.process_input(origin, input, actions, default_action);

    expect.equal(expected, res);
  });
});
