Module: %coil
Synopsis: An ordered table class to represent Coil structs.
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/// Synopsis: A table that keeps track of the order in which elements
///           are added.  Replaced keys retain their original ordering.
///           Iteration uses insertion order.
///
define open class <ordered-table> (<table>)
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
  next-method();
  if (~key-exists?(key, table))
    add!(table.key-sequence, key);
  end;
  new-value
end method element-setter;


define class <ordered-string-table> (<ordered-table>)
end;

define method key-test
    (t :: <ordered-string-table>) => (test :: <function>)
  \=
end;

define method table-protocol
    (table :: <ordered-string-table>)
 => (test :: <function>, hash :: <function>);
  values(\=, string-hash)
end method table-protocol;


// This is merely for consistency with Coil terminology.
define constant <struct> = <ordered-string-table>;

