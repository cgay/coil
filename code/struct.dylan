Module: %coil
Synopsis: An ordered table class to represent Coil structs.
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/// Synopsis: A table that keeps track of the order in which elements
///           are added.  Replaced keys retain their original ordering.
///           Iteration uses insertion order.
///
define open class <ordered-table> (<table>)
  slot key-sequence :: <list> = #();
  slot last-pair :: <list> = #();
end;

define method forward-iteration-protocol
    (c :: <ordered-table>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(0,                  // initial state
         c.size,             // limit
         // next state
         method (t :: <ordered-table>, index :: <integer>) => (state :: <integer>)
           index + 1
         end,
         // finished-state?
         method (t :: <ordered-table>, state :: <integer>, limit :: <integer>)
             => (finished? :: <boolean>)
           state = limit
         end,
         // current-key
         method (t :: <ordered-table>, state :: <integer>) => (key :: <object>)
           t.key-sequence[state]
         end,
         // current-element
         method (t :: <ordered-table>, state :: <integer>) => (key :: <object>)
           t[t.key-sequence[state]]
         end,
         // current-element-setter
         method (value :: <object>, t :: <ordered-table>, state :: <integer>)
             => (value :: <object>)
           t[t.key-sequence[state]] := value
         end,
         // copy-state
         method (t :: <ordered-table>, state :: <integer>) => (state :: <integer>)
           state
         end)
end method forward-iteration-protocol;

define method element-setter
    (new-value :: <object>, table :: <ordered-table>, key :: <object>)
 => (new-value :: <object>)
  if (~key-exists?(table, key))
    if (empty?(table.key-sequence))
      table.key-sequence := list(key);
      table.last-pair := table.key-sequence;
    else
      let new-last-pair = list(key);
      table.last-pair.tail := new-last-pair;
      table.last-pair := new-last-pair;
    end;
  end;
  next-method();
  new-value
end method element-setter;

// Update the key-sequence when keys are removed.
//
define method remove-key!
    (table :: <ordered-table>, key :: <object>) => (present? :: <boolean>)
  let keys-equal? :: <function> = table.key-test;
  let prev = #f;
  iterate loop (keys = table.key-sequence, prev = #f)
    if (~keys.empty?)
      let curr = keys.head;
      let rest = keys.tail;
      if (keys-equal?(curr, key))
        if (prev)
          prev.tail := rest;
          if (rest.empty?)      // Deleted last element.
            table.last-pair := prev;
          end;
        else                    // Deleting 1st element.
          table.key-sequence := rest;
          if (rest.empty?)      // Deleted last element.
            table.last-pair := #();
          end;
        end;
      else
        loop(rest, keys);
      end;
    end if;
  end;
  next-method()
end method remove-key!;

define class <ordered-string-table> (<ordered-table>)
end;

// This must be consistent with table-protocol.
define method key-test
    (t :: <ordered-string-table>) => (test :: <function>)
  \=
end;

// The first value returned must be consistent with key-test.
define method table-protocol
    (table :: <ordered-string-table>)
 => (test :: <function>, hash :: <function>);
  values(\=, string-hash)
end method table-protocol;


////
//// Struct
////


/// Synopsis: The core Coil data structure.  Back links to the parent struct
///           are maintained so that absolute references (i.e., @root...)
///           can be determined.
define open class <struct> (<ordered-string-table>)
  slot struct-parent :: false-or(<struct>) = #f,
    init-keyword: parent:;
  slot struct-name :: <string>,
    required-init-keyword: name:;
end class <struct>;

define method print-object
    (struct :: <struct>, stream :: <stream>)
 => ()
  format(stream, "<struct %s (%d item%s)>",
         struct.struct-full-name,
         struct.size,
         iff(struct.size = 1, "", "s"));
end;

define method struct-full-name
    (struct :: <struct>) => (full-name :: <string>)
  iff(struct.struct-parent,
      concatenate(struct-full-name(struct.struct-parent), ".", struct.struct-name),
      struct.struct-name)
end;

/// Synopsis: Return #t if the first argument is a descendant of the second.
///
define method descendant?
    (putative-descendant :: <struct>, struct :: <struct>)
 => (descendant? :: <boolean>)
  let parent = putative-descendant.struct-parent;
  ~(parent | struct.struct-parent)  // Both are #f?  i.e., @root extends @root
  | (parent
     & (putative-descendant == struct | descendant?(parent, struct)))
end;

define method find-root
    (struct :: <struct>) => (root :: <struct>)
  iff(struct.struct-parent,
      find-root(struct.struct-parent),
      struct)
end;

define constant $internal-unfound = list("iUNFOUND");

/// Synopsis: Retrieve an attribute value from a <struct>.
///
/// Arguments:
///     struct - A <struct>
///     key    - The name of the attribute to retrieve.  This may be any
///              valid coil path name.  e.g., foo, @root.bar.foo, bar.foo
///              If @root is part of the path then first the root struct
///              is looked up via the parent chain and then the path is
///              followed back down to the target.
///
define method element
    (struct :: <struct>, key :: <string>, #key default = $internal-unfound)
 => (object)
  local method find-root (s)
          iff(s.struct-parent, find-root(s.struct-parent), s)
        end;
  if (member?('.', key))
    iterate loop (struct = struct,
                  path = as(<list>, split(key, '.')),
                  seen = #())
      let subkey = path.head;
      let value = element(struct, subkey, default: $internal-unfound);
      if (value == $internal-unfound)
        if (default == $internal-unfound)
          let extra = iff(empty?(seen),
                          "",
                          format-to-string(" (because %= doesn't exist)",
                                           join(path, ".")));
          error(make(<invalid-key-error>,
                     format-string: "The key %= does not exist%s.",
                     format-arguments: list(key, extra)));
        else
          default
        end
      elseif (path.size = 1)
        value
      elseif (~instance?(value, <struct>))
        if (supplied?(default))
          // This could conceivably be an error
          default
        else
          error(make(<invalid-key-error>,
                     format-string: "The key %= does not exist. (%= is not a struct.)",
                     format-arguments: list(key, join(path, "."))));
        end
      else
        loop(value, path.tail, pair(subkey, seen))
      end
    end
  elseif (key = "@root")
    find-root(struct)
  else
    next-method()
  end
end method element;

define method deep-copy
    (struct :: <struct>, #key seen :: <list> = #(), signaler = error)
 => (struct :: <struct>)
  if (member?(struct, seen))
    signaler("Struct cycle detected: %s",
             join(reverse(seen), " -> ", key: struct-full-name));
  else
    let new = make(<struct>,
                   name: struct.struct-name,
                   parent: struct.struct-parent);
    for (value keyed-by key in struct)
      new[key] := if (instance?(value, <struct>))
                    deep-copy(value,
                              seen: pair(struct, seen),
                              signaler: signaler);
                  else
                    value
                  end;
    end;
    new
  end
end method deep-copy;


//// Outputting coil

define open generic write-coil
    (stream :: <stream>, coil-data) => ();

define method write-coil
    (stream :: <stream>, struct :: <struct>) => ()
  printing-logical-block (stream, prefix: "{", suffix: "}")
    for (value keyed-by key in struct)
      write-coil(stream, value);
    end;
  end;
end;

define method write-coil
    (stream :: <stream>, seq :: <sequence>) => ()
  printing-logical-block (stream, prefix: "[", suffix: "]")
    for (value in seq)
      write-coil(stream, value);
    end;
  end;
end;

define method write-coil
    (stream :: <stream>, int :: <integer>) => ()
  write(stream, integer-to-string(int));
end;

define method write-coil
    (stream :: <stream>, float :: <float>) => ()
  write(stream, float-to-string(float));
end;

define constant $newline-regex :: <regex> = compile-regex("\r|\r\n|\n");

define method write-coil
    (stream :: <stream>, string :: <string>) => ()
  if (member?('\n', string))
    printing-logical-block(stream, prefix: "\"\"\"", suffix: "\"\"\"")
      for (line in split(string, $newline-regex))
        write(stream, line);
      end;
    end;
  else
    format(stream, "%=", string);
  end;
end;

