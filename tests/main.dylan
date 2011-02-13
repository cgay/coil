Module: coil-test-suite
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


define suite coil-test-suite ()
  suite parser-test-suite;
  suite struct-test-suite;
  test test-ordered-table-fip;
end;

/// Synopsis: Make sure that the element-setter of the forward-iteration-protocol
///           for
define test test-ordered-table-fip ()
  let t = make(<struct>, name: "s1");
  t["foo"] := 1;
  t["bar"] := 2;
  t["baz"] := 3;
  check-equal("key-sequence has 3 elements", t.key-sequence.size, 3);

  let t2 = make(<struct>, name: "s2");
  map-into(t2, method(x) x + 1 end, t);
  check-equal("foo", t2["foo"], 2);
  check-equal("bar", t2["bar"], 3);
  check-equal("baz", t2["baz"], 4);

  let (initial-state, limit, next-state, finished-state?,
       current-key, current-element, current-element-setter, copy-state)
    = forward-iteration-protocol(t2);
  current-element-setter(10, t2, initial-state);
  check-equal("current element set", current-element(t2, initial-state), 10);
end test test-ordered-table-fip;

define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "coil-test-suite")
    run-test-application(coil-test-suite);
  end;
  // temp bug work-around
  force-output(*standard-output*);
end;

main();


