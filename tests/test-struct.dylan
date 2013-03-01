Module: coil-test-suite
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define suite struct-test-suite ()
  test test-forward-iteration-protocol;
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
