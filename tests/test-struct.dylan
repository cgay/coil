Module: coil-test-suite
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define suite struct-test-suite ()
  test test-lookup-dotted-path;
  test test-forward-iteration-protocol;
  test test-write-coil;
end;

define test test-lookup-dotted-path ()
  let root = make(<struct>);
  let a = make(<struct>);
  let b = make(<struct>);
  root["a"] := a;
  a["b"] := b;
  b["c"] := 9;
  check-equal("can lookup dotted path", root["a.b.c"], 9);
end;

define test test-forward-iteration-protocol ()
  let struct = make(<struct>, name: "foo");
  struct["aaa"] := 1;
  struct["bbb"] := 2;
  struct["ccc"] := 3;
  let (init, limit, next, finished?, current-key, current-element,
       current-element-setter, copy-state)
    = forward-iteration-protocol(struct);
  check-equal("init", init, 0);
  check-equal("curr key", current-key(struct, init), "aaa");
  check-equal("curr elem", current-element(struct, init), 1);
  check-equal("curr elem set 1", current-element-setter(100, struct, init), 100);
  check-equal("curr elem set 2", current-element(struct, init), 100);
  check-equal("limit", limit, 3);
  check-equal("next", next(struct, init), 1);

  //check-equal("map", map(identity, struct), #[100, 2, 3]);
end test test-forward-iteration-protocol;


define test test-write-coil ()
  local method to-string(x)
          with-output-to-string (s) write-coil(s, x) end
        end;
  check-equal("int", to-string(2), "2");
  check-equal("simple string", to-string("s"), "\"s\"");
  check-equal("string with newline", to-string("s\nt"), "\"\"\"s\nt\"\"\"");
  check-equal("list", to-string(#(1, 2)), "[1 2]");
  check-equal("vector", to-string(#[3, 4]), "[3 4]");

  let struct = make(<struct>);
  struct["c"] := 3;
  struct["b"] := 2;
  struct["a"] := 1;
  check-equal("struct1", to-string(struct), "c: 3\nb: 2\na: 1\n");
end test-write-coil;
