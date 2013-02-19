Module: coil-test-suite
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define constant fmt :: <function> = format-to-string;

/*
// uncomment for debugging
define method element-setter
    (new-value :: <object>, struct :: <struct>, key :: <object>)
 => (new-value :: <object>)
  test-output("Setting %s.%s to %=\n", struct.struct-full-name, key, new-value);
  next-method()
end;
*/

