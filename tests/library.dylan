Module: dylan-user
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define library coil-test-suite
  use common-dylan;
  use io;
  use system;
  use testworks;
  use coil;
end library coil-test-suite;

define module coil-test-suite
  use common-dylan;
  use locators;
  use standard-io;
  use streams;
  use testworks;
  use %coil;
end module coil-test-suite;

