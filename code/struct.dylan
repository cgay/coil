Module: %coil
Synopsis: An ordered table class to represent Coil structs.
Author: Carl Gay
Copyright: Copyright (c) 2011 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/// Synopsis: A table that keeps track of the order in which elements
///           are added.  Replaced keys retain their original ordering.
///           Iteration uses insertion order.
///
define open class <ordered-table> (<table>)
  // TODO: This should be a doubly-linked list so that deletion is cheaper.
  constant slot key-sequence :: <stretchy-vector> = make(<stretchy-vector>);
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
    add!(table.key-sequence, key);
  end;
  next-method();
  new-value
end method element-setter;


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

define constant $delete :: <string>
  = "@delete";


/// Synopsis: The core Coil data structure.  Back links to the parent struct
///           are maintained so that absolute references (i.e., @root...)
///           can be determined.
define open class <struct> (<ordered-string-table>)
  slot struct-parent :: false-or(<struct>) = #f,
    init-keyword: parent:;
  slot struct-name :: <string>,
    required-init-keyword: name:;
end class <struct>;

define method full-name
    (struct :: <struct>) => (full-name :: <string>)
  iff(struct.struct-parent,
      concatenate(full-name(struct.struct-parent), ".", struct.struct-name),
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
    (struct :: <struct>, key :: <string>, #key default = $unfound)
 => (object)
  if (member?('.', key))
    iterate loop (struct = struct,
                  parts = as(<list>, split(key, '.')),
                  seen = #())
      let subkey = parts.head;
      if (subkey = "@root" & ~empty?(seen))
        error("@root may only appear at the beginning of a coil path: %s", key);
      end;
      let value = element(struct, subkey, default: default);
      if (parts.size = 1)
        value
      elseif (~instance?(value, <struct>))
        error("%s is an invalid reference because %s does not name a struct.",
              key, join(reverse(seen), "."));
      else
        loop(value, parts.tail, pair(subkey, seen))
      end
    end
  elseif (key = "@root")
    iterate loop (struct = struct)
      iff(struct.struct-parent,
          loop(struct.struct-parent),
          struct)
    end
  else
    next-method()
  end
end method element;

define method deleted?
    (struct :: <struct>, key) => (deleted? :: <boolean>)
  member?(key, element(struct, $delete, default: #[]), test: \=)
end;


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

