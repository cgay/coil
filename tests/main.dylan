Module: coil-test-suite
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


define suite coil-test-suite ()
  suite parser-test-suite;
  suite struct-test-suite;
end;

define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "coil-test-suite")
    run-test-application(coil-test-suite);
  end;
  // temp bug work-around
  force-output(*standard-output*);
end;

main();


