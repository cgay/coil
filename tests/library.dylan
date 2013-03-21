Module: dylan-user
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define library coil-test-suite
  use common-dylan;
  use io,
    import: { format, standard-io, streams };
  use system,
    import: { file-system, locators };
  use testworks;
  use coil;
  use regular-expressions;
end library coil-test-suite;

define module coil-test-suite
  use coil;
  use %coil;
  use common-dylan, exclude: { format-to-string };
  use file-system,
    prefix: "fs/";
  use format;
  use locators;
  use regular-expressions;
  use standard-io;
  use streams;
  use testworks;
end module coil-test-suite;

