open TestFramework;
open Ace;
open Base;

describe("parse_command", ({test}) => {
  test("Parse a valid command", ({expect}) => {
    let input = "!cmd";

    let res = Ace.Parser.parse_command(input);

    expect.result(res).toBeOk();
    expect.equal(("!cmd", []), Result.ok_or_failwith(res))

  });

  test("Parse a valid command with arguments", ({expect}) => {
    let input = "!cmd one two three";

    let res = Ace.Parser.parse_command(input);

    expect.result(res).toBeOk();
    expect.equal(("!cmd", ["one", "two", "three"]), Result.ok_or_failwith(res))
  });

  test("Parse an invalid command", ({expect}) => {
    let input = "!!invalidcmd";

    let res = Ace.Parser.parse_command(input);

    expect.result(res).toBeError();
  });

});
