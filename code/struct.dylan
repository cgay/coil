Module: %coil
Synopsis: An ordered table class to represent Coil structs.
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/// Synopsis: The core Coil data structure.  Maintains an ordered mapping
///   of key/value pairs. Back links to the parent struct are maintained
///   so that absolute references (i.e., @root...) can be determined.
///
define open class <struct> (<mutable-explicit-key-collection>)
  slot struct-entries :: <list> = #();
  slot struct-parent :: false-or(<struct>) = #f,
    init-keyword: parent:;
  constant slot struct-name :: <string>,
    required-init-keyword: name:;
end class <struct>;

define method print-object
    (struct :: <struct>, stream :: <stream>)
 => ()
  format(stream, "<struct %s (%d item%s)>",
         struct.struct-full-name,
         struct.struct-entries.size,
         iff(struct.size = 1, "", "s"));
end;

define class <entry> (<object>)
  constant slot entry-key :: <string>, required-init-keyword: key:;
  slot entry-value :: <object>, required-init-keyword: value:;
end;

define inline function find-entry
    (lst :: <list>, key :: <string>)
 => (entry :: false-or(<entry>), index :: <integer>)
  iterate loop (lst = lst, index = 0)
    if (empty?(lst))
      values(#f, -1)
    else
      let entry = lst.head;
      if (entry.entry-key = key)
        values(entry, index)
      else
        loop(lst.tail, index + 1)
      end
    end
  end
end function find-entry;

define method forward-iteration-protocol
    (c :: <struct>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(0,                  // initial state
         c.size,             // limit
         // next state
         method (t :: <struct>, index :: <integer>) => (state :: <integer>)
           index + 1
         end,
         // finished-state?
         method (t :: <struct>, state :: <integer>, limit :: <integer>)
             => (finished? :: <boolean>)
           state = limit
         end,
         // current-key
         method (t :: <struct>, state :: <integer>) => (key :: <object>)
           t.struct-entries[state].entry-key
         end,
         // current-element
         method (t :: <struct>, state :: <integer>) => (key :: <object>)
           t.struct-entries[state].entry-value
         end,
         // current-element-setter
         method (value :: <object>, t :: <struct>, state :: <integer>)
             => (value :: <object>)
           t.struct-entries[state].entry-value := value
         end,
         // copy-state
         method (t :: <struct>, state :: <integer>) => (state :: <integer>)
           state
         end)
end method forward-iteration-protocol;

define method key-sequence
    (struct :: <struct>) => (v :: <vector>)
  let v = make(<vector>, size: struct.size);
  for (i from 0 below v.size,
       entry in struct.struct-entries)
    v[i] := entry.entry-key
  end;
  v
end method key-sequence;

define method size
    (struct :: <struct>) => (size :: <integer>)
  struct.struct-entries.size
end;

define method element-setter
    (new-value :: <object>, struct :: <struct>, key :: <string>)
 => (new-value :: <object>)
  let entry = find-entry(struct.struct-entries, key);
  if (entry)
    entry.entry-value := new-value;
  else
    let entry = make(<entry>, key: key, value: new-value);
    struct.struct-entries := add!(struct.struct-entries, entry);
  end;
  new-value
end method element-setter;

define method remove-key!
    (struct :: <struct>, key :: <object>) => (present? :: <boolean>)
  let content :: <list> = struct.struct-entries;
  let (entry, index) = find-entry(content, key);
  if (entry)
    // TODO(cgay): Why isn't remove-key! defined for <stretchy-vector> or <list>?
    struct.struct-entries
      := concatenate(copy-sequence(content, end: index),
                     copy-sequence(content, start: index + 1));
    #t
  end
end method remove-key!;

define method key-test
    (t :: <struct>) => (test :: <function>)
  \=
end;

define method struct-full-name
    (struct :: <struct>) => (full-name :: <string>)
  iff(struct.struct-parent,
      concatenate(struct.struct-parent.struct-full-name, ".", struct.struct-name),
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
      new[key] := iff(instance?(value, <struct>),
                      deep-copy(value,
                                seen: pair(struct, seen),
                                signaler: signaler),
                      value);
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

