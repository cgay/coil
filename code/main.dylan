Module: %coil
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
Synopsis: A lot of the code in this file was translated directly from the Python coil code.
License:   See LICENSE.txt in this distribution for details.

/// Synopsis: Major, minor, and patch versions of this library.
///           The major version should change when/if the format
///           changes incompatibly.
///
define constant $coil-major-version :: <integer> = 0;
define constant $coil-minor-version :: <integer> = 1;
define constant $coil-patch-version :: <integer> = 0;

define constant $internal-default = list(0);

// Because I hate typing format-string: and format-arguments: again and again...
define function raise
    (class :: <class>, format-string :: <string>, #rest format-arguments) => ()
  signal(make(class,
              format-string: format-string,
              format-arguments: format-arguments));
end;

