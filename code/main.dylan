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

define method class-name
    (struct :: <struct-prototype>) => (name :: <string>)
  "struct-prototype"
end;

// Look up a simple key (not a path).  Locally set values (i.e.,
// values set in the <struct> superclass) take precedence but if not
// found then look at inherited values (unless locally deleted).
define method %element
    (struct :: <struct-prototype>, key :: <string>, default :: <object>)
 => (object)
  if (member?(key, struct.deleted-keys, test: \=))
    default
  else
    let value = next-method();
    if (value == $internal-default)
      element(struct.secondary-values, key, default: default)
    else
      value
    end
  end
end method %element;

define method size
    (struct :: <struct-prototype>) => (size :: <integer>)
  let all-keys = union(struct.struct-order, struct.secondary-order, test: \=);
  all-keys.size - intersection(all-keys, struct.deleted-keys, test: \=).size
end;

define method copy-struct-into
    (source :: <struct-prototype>, target :: <struct>) => ()
  // Copy secondary items in first, excluding those that are overridden
  // by items explicitly set for this struct, rather than inherited.

  // TODO(cgay): This gets the order wrong if @extends or @file
  // wasn't the first thing in the struct!  Enforce that?  Also
  // might want to note in the docs that order isn't defined for
  // multiple @s in the same struct.
  for (key in source.secondary-order)
    if (~member?(key, source.deleted-keys, test: \=)
          & ~member?(key, source.struct-order, test: \=))
      format-out("copying secondary %= = %=\n", key, source.secondary-values[key]);
      target[key] := source.secondary-values[key];
    end;
  end;
  for (key in source.struct-order)
    let value = source.struct-values[key];
    format-out("copying primary %= = %=\n", key, value);
    target[key] := select (value by instance?)
                     <struct> =>
                       let st = make(<struct>);
                       copy-struct-into(value, st);
                       st;
                     otherwise => value;
                   end;
  end;
end method copy-struct-into;
