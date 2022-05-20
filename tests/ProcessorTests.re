/* open Base; */
/* open Ace; */
/* open Lwt; */
/* open Core.Types; */
/* open TestFramework; */
/*  */
/* describe("Processor.parse_command", ({test}) => { */
/*   test("Parse a valid command", ({expect}) => { */
/*     let input = "!cmd"; */
/*  */
/*     let res = Processor.parse_command(input); */
/*  */
/*     expect.result(res).toBeOk(); */
/*     expect.equal(("cmd", []), Result.ok_or_failwith(res)); */
/*   }); */
/*  */
/*   test("Parse a valid command with arguments", ({expect}) => { */
/*     let input = "!cmd one two three"; */
/*  */
/*     let res = Processor.parse_command(input); */
/*  */
/*     expect.result(res).toBeOk(); */
/*     expect.equal( */
/*       ("cmd", ["one", "two", "three"]), */
/*       Result.ok_or_failwith(res), */
/*     ); */
/*   }); */
/*  */
/*   test("Parse an invalid command", ({expect}) => { */
/*     let input = "!!invalidcmd"; */
/*  */
/*     let res = Processor.parse_command(input); */
/*  */
/*     expect.result(res).toBeError(); */
/*   }); */
/* }); */
/*  */
/* describe("Processor.process_input", ({test}) => { */
/*   test("Process command", ({expect}) => { */
/*     let input = "!test"; */
/*  */
/*     let res = Processor.process_input(input); */
/*  */
/*     expect.result(res).toBe(Ok(Input.Command(input, "test", []))); */
/*   }); */
/*  */
/*   test("Process a command with arguments", ({expect}) => { */
/*     let input = "!ping one two"; */
/*  */
/*     let res = Processor.process_input(input); */
/*  */
/*     expect.result(res).toBe( */
/*       Ok(Input.Command(input, "ping", ["one", "two"])), */
/*     ); */
/*   }); */
/*  */
/*   test("Process a sentence", ({expect}) => { */
/*     let input = "hello world!"; */
/*  */
/*     let res = Processor.process_input(input); */
/*  */
/*     expect.result(res).toBe(Ok(Input.Sentence("hello world!"))); */
/*   }); */
/*  */
/*   test("Process a bad formated command", ({expect}) => { */
/*     let input = "!!test"; */
/*  */
/*     let res = Processor.process_input(input); */
/*  */
/*     expect.result(res).toBeError(); */
/*   }); */
/*  */
/*   test("Process a command with missing command name", ({expect}) => { */
/*     let input = "! "; */
/*  */
/*     let res = Processor.process_input(input); */
/*  */
/*     expect.result(res).toBeError(); */
/*   }); */
/* }); */
/*  */
/* describe("Processor.find_action_or_default", ({test}) => { */
/*   let ping_action = */
/*     Action.{ */
/*       name: "Ping response", */
/*       from: Origin.Shell, */
/*       on: [Event.Command("ping")], */
/*       runner: Action.Runner.DirectResponse, */
/*     }; */
/*   let actions = [|ping_action|]; */
/*   let default = */
/*     Action.{ */
/*       name: "Unknown", */
/*       from: Origin.Shell, */
/*       on: [], */
/*       runner: Action.Runner.DirectResponse, */
/*     }; */
/*  */
/*   test("find ping action", ({expect}) => { */
/*     let incoming = */
/*       Incoming.{ */
/*         input: Input.Command("!ping", "ping", []), */
/*         origin: Origin.Shell, */
/*         destination: Origin.Shell, */
/*       }; */
/*  */
/*     let res = Processor.find_action_or_default(incoming, actions, default); */
/*  */
/*     expect.equal((ping_action, Some(Event.Command("ping"))), res); */
/*   }); */
/*  */
/*   test("Find no action and get the default action", ({expect}) => { */
/*     let incoming = */
/*       Incoming.{ */
/*         input: Input.Command("!unknown", "unknown", []), */
/*         origin: Origin.Shell, */
/*         destination: Origin.Shell, */
/*       }; */
/*  */
/*     let res = Processor.find_action_or_default(incoming, actions, default); */
/*  */
/*     expect.equal((default, None), res); */
/*   }); */
/*  */
/*   test( */
/*     "Find the first matching action for multiple candidate actions", */
/*     ({expect}) => { */
/*     let ping_action = */
/*       Action.{ */
/*         name: "Ping response", */
/*         from: Origin.Shell, */
/*         on: [Event.Command("ping")], */
/*         runner: Action.Runner.DirectResponse, */
/*       }; */
/*     let second_action = */
/*       Action.{ */
/*         name: "Second response", */
/*         from: Origin.Shell, */
/*         on: [Event.Command("ping")], */
/*         runner: Action.Runner.DirectResponse, */
/*       }; */
/*     let actions = [|ping_action, second_action|]; */
/*     let incoming = */
/*       Incoming.{ */
/*         input: Input.Command("!ping", "ping", []), */
/*         origin: Origin.Shell, */
/*         destination: Origin.Shell, */
/*       }; */
/*  */
/*     let res = Processor.find_action_or_default(incoming, actions, default); */
/*  */
/*     expect.equal((ping_action, Some(Event.Command("ping"))), res); */
/*   }); */
/*  */
/*   describe("Processor.execute", ({test}) => { */
/*     test("Execute ping action", ({expect}) => { */
/*       let incoming = */
/*         Incoming.{ */
/*           input: Input.Command("!ping", "ping", []), */
/*           origin: Origin.Shell, */
/*           destination: Origin.Shell, */
/*         }; */
/*  */
/*       let action = */
/*         Action.{ */
/*           name: "Ping response", */
/*           from: Origin.Shell, */
/*           on: [Event.Command("ping")], */
/*           runner: Action.Runner.DirectResponse, */
/*         }; */
/*       let event = Some(Event.Command("ping")); */
/*  */
/*       let res = Lwt_main.run(Processor.execute(incoming, event, action)); */
/*  */
/*       expect.result(res).toBeOk(); */
/*       expect.equal( */
/*         Option.value_exn(Result.ok(res)), */
/*         (incoming, {action, event, response: Response.Ok_}), */
/*       ); */
/*     }) */
/*   }); */
/*  */
/*   test("Execute default action", ({expect}) => { */
/*     let incoming = */
/*       Incoming.{ */
/*         input: Input.Command("!unknown", "unknown", []), */
/*         origin: Origin.Shell, */
/*         destination: Origin.Shell, */
/*       }; */
/*  */
/*     let default = */
/*       Action.{ */
/*         name: "unknown", */
/*         from: Origin.Shell, */
/*         on: [], */
/*         runner: Action.Runner.DirectResponse, */
/*       }; */
/*  */
/*     let res = Lwt_main.run @@ Processor.execute(incoming, None, default); */
/*  */
/*     expect.result(res).toBeOk(); */
/*     expect.equal( */
/*       Option.value_exn(Result.ok(res)), */
/*       ( */
/*         incoming, */
/*         { */
/*           action: default, */
/*           event: None, */
/*           response: Response.Error("Unknown command"), */
/*         }, */
/*       ), */
/*     ); */
/*   }); */
/*   test("Execute invalid action", ({expect}) => { */
/*     let incoming = */
/*       Incoming.{ */
/*         input: Input.Command("!invalid", "invalid", []), */
/*         origin: Origin.Shell, */
/*         destination: Origin.Shell, */
/*       }; */
/*  */
/*     let action = */
/*       Action.{ */
/*         name: "invalid", */
/*         from: Origin.Shell, */
/*         on: [], */
/*         runner: Action.Runner.DirectResponse, */
/*       }; */
/*     let event = Some(Event.Command("invalid")); */
/*  */
/*     let res = Lwt_main.run @@ Processor.execute(incoming, event, action); */
/*  */
/*     expect.result(res).toBe(Error("Command execution failed")); */
/*   }); */
/* }); */
