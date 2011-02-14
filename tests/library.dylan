Module: dylan-user
Author: Carl Gay
Copyright: Copyright (c) 2011 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define library coil-test-suite
  use common-dylan;
  use io;
  use system;
  use testworks;
  use coil;
  use regular-expressions;
end library coil-test-suite;

define module coil-test-suite
  use common-dylan, exclude: { format-to-string };
  use format;
  use regular-expressions;
  use locators;
  use standard-io;
  use streams;
  use testworks;
  use coil;
  use %coil;
end module coil-test-suite;

