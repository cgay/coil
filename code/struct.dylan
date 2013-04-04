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

define method struct-name
    (struct :: <struct>) => (name :: <string>)
  struct.%struct-name | "???"
end;

// I wish support for this were exported from the dylan library, but
// it doesn't seem to be.
define sealed generic class-name
    (struct :: <struct>) => (name :: <string>);

define method class-name
    (struct :: <struct>) => (name :: <string>)
  "struct"
end;

define method print-object
    (struct :: <struct>, stream :: <stream>)
 => ()
  format(stream, "<%s %s (%d item%s)>",
         struct.class-name,
         struct.struct-full-name,
         struct.size,
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
           error("current-element-setter not defined");
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
  let (node :: <struct>, key :: <string>) = find-penultimate(key-or-path, struct);
  add-new!(node.struct-order, key, test: \=);
  node.struct-values[key] := new-value;
  if (instance?(new-value, <struct>) & ~new-value.struct-parent)
    new-value.struct-parent := node;
  end;
  new-value
end method element-setter;

define method remove-key!
    (struct :: <struct>, key-or-path :: <string>) => (present? :: <boolean>)
  let (node :: <struct>, key :: <string>) = find-penultimate(key-or-path, struct);
  let present? = #f;
  remove!(node.struct-order, key, test: \=);
  remove-key!(node.struct-values, key)
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

// TODO(cgay): use unfound() instead
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
    (struct :: <struct>, key-or-path :: <string>, #key default = $internal-unfound)
 => (object)
  format-out("element(%=, %=)\n", struct, key-or-path);
  let (node :: <struct>, key :: <string>) = find-penultimate(key-or-path, struct);
  format-out("  node = %=\n", node);
  let value = %element(node, key, default);
  if (value == $internal-unfound)
    raise(<key-error>, "Key %= not found.", key);
  else
    value
  end
end method element;

// Gives <struct-prototype> a place to override.  This only handles
// simple keys, not paths.
define sealed generic %element
    (struct :: <struct>, key :: <string>, default :: <object>)
 => (object);

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

define method find-penultimate
    (path :: <string>, struct :: <struct>)
 => (struct :: <struct>, simple-key :: <string>)
  format-out("find-penultimate(%=, %=)\n", path, struct);
  if (starts-with?(path, "."))
    // .. = parent, ... = grandparent, etc
    path := copy-sequence(path, start: 1);  
  end;
  iterate loop (current = struct, elements = as(<list>, split(path, '.')), seen = #())
    format-out("  current = %=, elements = %=, seen = %=\n", current, elements, seen);
    if (empty?(elements.tail))
      format-out("  => %=\n", current);
      values(current, elements.head)
    else
      let simple-key = elements.head;
      let new-seen = pair(simple-key, seen);
      let value = select (simple-key by \=)
                    "@root" =>
                      iterate find-root (st = current)
                        iff(st.struct-parent, find-root(st.struct-parent), st)
                      end;
                    "" =>   // splitting on '.' removes the dots.
                      current.struct-parent;
                    otherwise =>
                      // Don't call element(current, ...) here because it would cause
                      // infinite recursion.
                      %element(current, simple-key, $internal-unfound);
                  end;
      if (value == $internal-unfound)
        raise(<key-error>,
              "Invalid struct path %s: %s does not exist.",
              path, join(reverse(new-seen), "."));
      elseif (~instance?(value, <struct>))
        raise(<key-error>,
              "Invalid struct path %s: %s does not name a struct.",
              path, join(reverse(new-seen), "."));
      end;
      loop(value, elements.tail, new-seen)
    end
  end
end method find-penultimate;
