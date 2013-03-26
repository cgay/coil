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

/// Synopsis: A temporary struct used for parsing only.  This Struct
/// tracks links and inheritance so they can be processed when parsing
/// is all done. This is important because it allows us to do fancy
/// things with inheritance and catch errors during parse-time rather
/// than run-time.
define class <struct-prototype> (<struct>)
  // Secondary items are ones that are inherited via @extends or @file
  // They must be tracked separately so we can raise errors on
  // double adds and deletes in the primary values.
  constant slot secondary-values :: <string-table> = make(<string-table>);
  constant slot secondary-order :: <stretchy-vector> = make(<stretchy-vector>);

  // Keys that were deleted in this struct by ~foo tokens.
  constant slot deleted-keys :: <stretchy-vector> = make(<stretchy-vector>);
end class <struct-prototype>;

define method %element
    (struct :: <struct-prototype>, key :: <string>, default :: <object>)
 => (object)
  if (member?(key, struct.deleted-keys, test: \=))
    default
  else
    let v = element(struct.struct-values, key, default: $internal-default);
    if (v == $internal-default)
      element(struct.secondary-values, key, default: default)
    else
      default
    end
  end
end method %element;



