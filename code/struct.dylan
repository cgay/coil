Module: %coil
Synopsis: An ordered table class to represent Coil structs.
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/// Synopsis: The core Coil data structure.  Maintains an ordered mapping
///   of key/value pairs. Back links to the parent struct are maintained
///   so that absolute references (i.e., @root...) can be determined.
///   Keys are strings.
///
define open class <struct> (<mutable-explicit-key-collection>)
  constant slot struct-values :: <string-table> = make(<string-table>);
  constant slot struct-order :: <stretchy-vector> = make(<stretchy-vector>);
  slot struct-parent :: false-or(<struct>) = #f,
    init-keyword: parent:;
  // This is for debugging only.  For the top-level struct in a file
  // you might want to set it to the filename.  Otherwise it's the
  // key under which the struct was created.
  constant slot %struct-name :: false-or(<string>) = #f,
    init-keyword: name:;
end class <struct>;

define method initialize
    (struct :: <struct>, #key copy-from :: false-or(<struct>))
  if (copy-from)
    // Recursively copy the given struct.  This is primarily intended
    // for replacing the <struct-prototype> parse tree with actual
    // structs.  It iterates over the keys rather than using "for (v
    // keyed-by k in prototype)" so as to explicitly call the
    // "element" method.
    let dict = copy-from.struct-values;
    for (key in copy-from.struct-order)
      let value = copy-from[key];
      dict[key] := select (value by instance?)
                     <struct> => make(<struct>, copy-from: value);
                     otherwise => value;
                   end;
    end;
  end;
end method initialize;

define method struct-name
    (struct :: <struct>) => (name :: <string>)
  struct.%struct-name | "???"
end;

define method print-object
    (struct :: <struct>, stream :: <stream>)
 => ()
  format(stream, "<struct %s (%d item%s)>",
         struct.struct-full-name,
         struct.struct-values.size,
         iff(struct.size = 1, "", "s"));
end;

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
         method (struct :: <struct>, state :: <integer>) => (key :: <object>)
           struct.struct-order[state]
         end,
         // current-element
         method (struct :: <struct>, state :: <integer>) => (key :: <object>)
           struct.struct-values[struct.struct-order[state]]
         end,
         // current-element-setter
         method (value :: <object>, struct :: <struct>, state :: <integer>)
             => (value :: <object>)
           struct.struct-values[state] := value
         end,
         // copy-state
         method (t :: <struct>, state :: <integer>) => (state :: <integer>)
           state
         end)
end method forward-iteration-protocol;

define method key-sequence
    (struct :: <struct>) => (v :: <vector>)
  map-as(<vector>, identity, struct.struct-order)
end method key-sequence;

define method size
    (struct :: <struct>) => (size :: <integer>)
  struct.struct-order.size
end;

// key may be a simple key or a dotted path.
define method element-setter
    (new-value :: <object>, struct :: <struct>, key-or-path :: <string>)
 => (new-value :: <object>)
  local method node-handler (node, simple-key, path)
          if (path == key-or-path)
            add-new!(node.struct-order, simple-key, test: \=);
            node.struct-values[simple-key] := new-value;
            if (instance?(new-value, <struct>) & ~new-value.struct-parent)
              new-value.struct-parent := node;
            end;
          end;
        end;
  do-path(key-or-path, struct, node-handler);
  new-value
end method element-setter;

define method remove-key!
    (struct :: <struct>, key-or-path :: <string>) => (present? :: <boolean>)
  let present? = #f;
  local method node-handler (node, simple-key, path)
          if (path == key-or-path)
            remove!(node.struct-order, simple-key, test: \=);
            present? := remove-key!(node.struct-values, simple-key);
          end;
        end;
  do-path(key-or-path, struct, node-handler);
  present?
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
  let result = default;
  local method node-handler (node, simple-key, path)
          if (path == key)
            add-new!(node.struct-order, simple-key);
            let val = %element(node, simple-key, default);
            if (val == $internal-unfound)
              raise(<key-error>, "Key %s not found.", key);
            else
              result := val
            end;
          end;
        end;
  do-path(key, struct, node-handler);
  result
end method element;

// Gives <struct-prototype> a place to override.  This only handles
// simple keys.
define method %element
    (struct :: <struct>, key :: <string>, default :: <object>)
 => (object)
  element(struct.struct-values, key, default: default)
end;

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

// Apply a function to each node in a dotted path.  Ensures that each
// non-terminal element of the path is a struct and optionally ensures
// that the terminal element is a struct.
define method do-path
    (path :: <string>, struct :: <struct>, node-handler :: <function>,
     #key require-struct? :: <boolean>)
 => ()
  iterate loop (parent = struct, elements = as(<list>, split(path, '.')), seen = #())
    let simple-key = elements.head;
    let new-seen = pair(simple-key, seen);
    let value = if (simple-key = "@root")
                  element(parent, simple-key, default: $internal-unfound)
                else
                  iterate find-root (st = parent)
                    iff(st.struct-parent, find-root(st.struct-parent), st)
                  end
                end;
    if (value == $internal-unfound)
      raise(<key-error>,
            "Invalid struct path %s: %s does not exist.",
            path, join(reverse(new-seen), "."));
    elseif ((require-struct? | (elements.size > 1)) & ~instance?(value, <struct>))
      raise(<key-error>,
            "Invalid struct path %s: %s does not name a struct.",
            path, join(reverse(new-seen), "."));
    end;
    node-handler(value, simple-key, join(reverse(new-seen), "."));
    loop(value, elements.tail, new-seen);
  end;
end method do-path;


//// Outputting coil

// TODO: Could consider eliding some nesting levels, as an option.
// For example, if a struct contains a single key/value pair which is
// itself a struct...
//   a: {
//     b: 6
//   }
// they can be rewritten as a compound key:
//   a.b: 6


define open generic write-coil
    (stream :: <stream>, coil-data, #key indent) => ();

define method write-coil
    (stream :: <stream>, struct :: <struct>, #key indent = "")
 => ()
  let root? = (indent = "");
  let prefix = if (root?) "" else "{ " end;
  let suffix = if (root?) "" else "}" end;
  printing-logical-block (stream, prefix: prefix, suffix: suffix)
    for (value keyed-by key in struct,
         first? = #t then #f)
      if (~(root? & first?))
        // Don't put a blank line at the beginning of a "file".
        write(stream, "\n");
      end;
      write(stream, indent);
      write(stream, key);
      write(stream, ": ");
      write-coil(stream, value, indent: concatenate("  ", indent));
    end;
    write(stream, "\n");
    write(stream, indent, end: max(0, indent.size - 2));
  end;
end method write-coil;

define method write-coil
    (stream :: <stream>, seq :: <sequence>, #key indent = "")
 => ()
  printing-logical-block (stream, prefix: "[", suffix: "]")
    for (value in seq,
         first? = #t then #f)
      if (~first?)
        write(stream, " ");
      end;
      write-coil(stream, value, indent: concatenate("  ", indent));
    end;
  end;
end method write-coil;

define method write-coil
    (stream :: <stream>, int :: <integer>, #key indent = "")
 => ()
  write(stream, integer-to-string(int));
end;

define method write-coil
    (stream :: <stream>, float :: <float>, #key indent = "")
 => ()
  write(stream, float-to-string(float));
end;

define method write-coil
    (stream :: <stream>, string :: <string>, #key indent = "")
 => ()
  let prefix = if (member?('\n', string)
                     | member?('\r', string)
                     | (member?('"', string) & member?('\'', string)))
                 "\"\"\""
               elseif (member?('"', string))
                 "'"
               else
                 "\""
               end;
  printing-logical-block(stream, prefix: prefix, suffix: prefix)
    write(stream, string);
  end;
end method write-coil;
