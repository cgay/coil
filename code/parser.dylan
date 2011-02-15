Module: %coil
Synopsis: Ad-hoc recursive descent parser for coil configs
Author: Carl Gay
Copyright: Copyright (c) 2011 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/*

This parser makes two passes.  During the first pass objects that
don't involve any links outside the struct are parsed, and most
struct keys are filled in.  The second pass resolves @extends,
@file, deletions, compound struct keys (i.e., a.b.c: foo) and
struct value links (i.e., foo: a.b.c).  This is necessary because
@extends and @file may change what a.b.c refers to.

During the first pass, keys begining with '@' are used as placeholders
for items that need to be resolved during the second pass.  This works
because '@' isn't a valid character in coil struct keys.

A basic rule is that structs must be parsed in textual order.  Structs
that appear in the source file textually before other structs should
be fully parsed before those that appear later.  e.g., if a struct has
both "x: y" and "~x" then the "winner" is whichever one appears last.

TODO:
+ What are the intended semantics when a struct has both x and ~x?

+ Keep track of the source location of items that are resolved during
  the second pass, for better error messages.

*/

define constant $whitespace :: <string>
  = " \t\n\r";

define constant $token-terminators :: <string>
  = concatenate($whitespace, "}]\"");

define constant $key-regex :: <regex>
  = compile-regex("-*[a-zA-Z_][\\w-]*");

define constant $extended-key-regex :: <regex>
  = compile-regex(format-to-string("%s(\\.%s)*",
                                   $key-regex.regex-pattern,
                                   $key-regex.regex-pattern));

define constant $path-regex :: <regex>
  = compile-regex(format-to-string("(@|\\.+)?%s(\\.%s)*",
                                   $key-regex.regex-pattern,
                                   $key-regex.regex-pattern));


/// Synopsis: '$none' is what "None" parses to.
///
define class <none> (<object>) end;
define constant $none :: <none> = make(<none>);

define class <coil-parser> (<object>)
  // Source is for error reporting only.  It could be a file name, a stream, etc.
  constant slot input-source :: <object>, required-init-keyword: source:;

  // Text is the entire original coil source text.
  constant slot input-text :: <string> = "", required-init-keyword: text:;

  // Index points to the next character to be read by "consume".
  slot current-index :: <integer> = 0;

  // Line and column are for error reporting.  They are maintained by "consume".
  slot line-number :: <integer> = 1, init-keyword: line:;
  slot column-number :: <integer> = 1, init-keyword: column:;
end class <coil-parser>;


/// Synopsis: Any error signalled during parsing (except for file
///           system errors) will be an instance of this.
define class <coil-parse-error> (<coil-error>)
end;


/// Synopsis: Signal <coil-parse-error> with the given 'format-string' and
///           'args' as the message.  If the current source location is known
///           it is prefixed to the message.
define method parse-error
    (p :: <coil-parser>, format-string, #rest args)
  let context = format-to-string("@%d:%d ", p.line-number, p.column-number);
  let message = concatenate(context,
                            apply(format-to-string, format-string, args),
                            "\n", p.current-line,
                            "\n", p.indicator-line);
  error(make(<coil-parse-error>, format-string: message));
end;

/// Synopsis: Return the line pointed to by 'current-index'.
///
define method current-line
    (p :: <coil-parser>) => (line :: <string>)
  let max = p.input-text.size;
  let curr = p.current-index;
  let epos = min(position(p.input-text, '\n', start: curr) | max,
                 position(p.input-text, '\r', start: curr) | max);
  slice(p.input-text,
        p.current-index - p.column-number + 1,
        epos)
end;

/// Synopsis: Return a line that indicates which character 'current-index'
///           points to.  ".........^"
define method indicator-line
    (p :: <coil-parser>) => (line :: <string>)
  let line = make(<string>, size: p.column-number, fill: '.');
  line[p.column-number - 1] := '^';
  line
end;

/// Synopsis: Parse coil formatted text from the given 'source'.
///           This is the main user-visible entry point for parsing.
define open generic parse-coil
    (source :: <object>) => (coil :: <struct>);

define method parse-coil
    (source :: <locator>) => (coil :: <struct>)
  with-open-file (stream = source, direction: input:)
    %parse-coil(make(<coil-parser>,
                     source: as(<string>, source),
                     text: read-to-end(stream)))
  end
end;

define method parse-coil
    (source :: <stream>) => (coil :: <struct>)
  %parse-coil(make(<coil-parser>, source: source, text: read-to-end(source)))
end;

define method parse-coil
    (source :: <string>) => (coil :: <struct>)
  %parse-coil(make(<coil-parser>, source: source, text: source))
end;

define method %parse-coil
    (p :: <coil-parser>) => (struct :: <struct>)
  let root = make(<struct>, name: "@root");
  resolve-references!(p, parse-struct-attributes(p, root), #());
  root
end;

define method parse-struct
    (p :: <coil-parser>, key :: <string>) => (struct :: <struct>)
  let struct = make(<struct>, name: key);
  eat-whitespace-and-comments(p);
  select (p.lookahead)
    '{' =>
      p.consume;
      parse-struct-attributes(p, struct);
      expect(p, "}");
    otherwise =>
      parse-error(p, "Invalid struct.  Expected '{'.");
  end;
  struct
end method parse-struct;

/// Synopsis: Parse the attributes of a struct and add them to the 'struct'
///           argument passed in.  @extends, @file, and deletions are added
///           to the struct under special names (which aren't valid as keys)
///           so that they may be resolved during a second pass.
define method parse-struct-attributes
    (p :: <coil-parser>, struct :: <struct>) => (struct :: <struct>)
  iterate loop (n = 1)
    eat-whitespace-and-comments(p);
    select (p.lookahead)
      '}', #f =>
        #f;  // done
      '~' =>
        p.consume;
        let link :: <link> = parse-link(p);
        struct[concatenate("@delete", link.link-name)] := link;
        loop(n);
      '@' =>
        p.consume;
        select (p.lookahead)
          'e' =>
            expect(p, "extends", ":");
            let key = format-to-string("@extends-%d", n);
            struct[key] := parse-link(p);
            loop(n + 1);
          'f' =>
            expect(p, "file", ":");
            let filename = parse-any(p);
            select (filename by instance?)
              <string> =>
                // @file: "foo.coil"
                let key = format-to-string("@file-%d", n);
                struct[key] := parse-coil(as(<file-locator>, filename));
                loop(n + 1);
              <sequence> =>
                // @file: [ "foo.coil" @root.a.b ]
                let (filename, path) = apply(values, filename);
                let key = format-to-string("@file-%d", n);
                let file-struct = parse-coil(as(<file-locator>, filename));
                let target :: <struct> = file-struct[path];
                target.struct-parent := struct;
                struct[key] := target;
                loop(n + 1);
              otherwise =>
                parse-error(p, "Expected a filename for @file but got %=",
                            filename);
            end select;
          'm' =>
            expect(p, "map", ":");
            parse-error(p, "@map is not supported.  Do you _really_ need it???");
          otherwise =>
            parse-error(p, "Unrecognized special attribute");
        end select;
        loop(n);
      otherwise =>
        let (key, simple-key) = parse-key(p);
        expect(p, "", ":", "");
        let value = parse-any(p, key: simple-key);
        struct[key] := value;
        if (instance?(value, <struct>) & ~value.struct-parent)
          value.struct-parent := struct;
        end;
        loop(n);
    end;
  end iterate;
  struct
end method parse-struct-attributes;


/// Synopsis: parse and return any valid coil object.  A struct, list,
///           integer, float, string, boolean, or None.  This is used for
///           parsing list elements and struct values, for example.
///           Note that this may only be called when an ENTIRE OBJECT is
///           expected, which specifically excludes parsing a path.
/// Arguments:
///   p   - Coil parser.
///   key - The key for which this value is being parsed.  This is used
///         to give a name to the struct created, if this method parses
///         a struct.  #f indicates that this is not called from
///         parse-struct-attributes, which is not allowed.
///
define method parse-any
    (p :: <coil-parser>, #key key :: false-or(<string>))
 => (object :: <object>)
  eat-whitespace-and-comments(p);
  let char = p.lookahead;
  select (char by member?)
    "'\"" =>
      parse-string(p);
    "{" =>
      if (~key)
        parse-error(p, "Structs are not allowed inside of lists.");
      end;
      parse-struct(p, key);
    "[" =>
      parse-list(p);
    "-0123456789" =>
      parse-number(p);
    // TODO: These three aren't right because they could be TrueXXX etc.
    //       That's one reason a real tokenizer separate from the parser
    //       would be better.
    "T" =>
      expect(p, "True");
      #t;
    "F" =>
      expect(p, "False");
      #f;
    "N" =>
      expect(p, "None");
      $none;
    ".@" =>
      parse-link(p);
    otherwise =>
      parse-error(p, "Unexpected input starting with %=.", char);
  end select
end method parse-any;

/// Synopsis: Return the next unread input character, or #f if at end.
define method lookahead
    (p :: <coil-parser>, #key offset :: <integer> = 0)
 => (char :: false-or(<character>))
  let text = p.input-text;
  let idx = p.current-index;
  if (idx + offset >= text.size)
    #f
  else
    text[idx + offset]
  end
end method lookahead;

/// Synopsis: Consume and return the next unread input character.  If at
///           end-of-input signal <coil-parse-error>.
define method consume
    (p :: <coil-parser>) => (char :: false-or(<character>))
  let char = p.lookahead;
  if (char)
    inc!(p.current-index);
    if (char = '\n')
      inc!(p.line-number);
      p.column-number := 1;
    else
      inc!(p.column-number);
    end;
    char
  else
    parse-error(p, "End of coil text encountered.");
  end;
end method consume;

define method expect
    (p :: <coil-parser>, #rest strings :: <string>) => ()
  for (string in strings)
    let start = p.current-index;
    for (char in string)
      if (char = p.lookahead)
        p.consume
      else
        parse-error(p, "Expected %= but got %=",
                    string, slice(p.input-text, start, p.current-index));
      end;
    end;
    eat-whitespace(p);
  end;
end method expect;

define method eat-whitespace-and-comments
    (p :: <coil-parser>) => ()
  iterate loop ()
    if (p.lookahead = '#')
      eat-comment(p);
      loop()
    elseif (member?(p.lookahead, $whitespace))
      p.consume;
      loop()
    end;
  end;
end;

/// Synopsis: Consume until not looking at a whitespace char.
///
define method eat-whitespace
    (p :: <coil-parser>) => ()
  while (member?(p.lookahead, $whitespace))
    p.consume;
  end;
end;

/// Synopsis: Consume a comment that starts with '#' and ends with '\n'.
///
define method eat-comment
    (p :: <coil-parser>) => ()
  while (p.consume ~= '\n')
  end;
end;

/// Synopsis: Parse a struct key, which may be a simple identifier such
///           as "a" or a compound identifier such as "a.b.c".  Compound
///           identifiers are resolved during the second pass.
define method parse-key
    (p :: <coil-parser>)
 => (key :: <string>, simple-key :: false-or(<string>))
  let match = regex-search($extended-key-regex, p.input-text,
                           start: p.current-index);
  if (match)
    let (key, _, epos) = match-group(match, 0);
    p.current-index := epos;
    adjust-column-number(p);
    if (member?('.', key))
      values(make-compound-key(key),
             elt(split(key, '.'), -1))
    else
      values(key, key)
    end
  else
    parse-error(p, "Struct key expected");
  end
end method parse-key;

define method make-compound-key
    (key :: <string>) => (compound-key :: <string>)
  // This feels a bit hackish, but is necessary because dot ('.') is treated
  // specially by element(<struct>, foo).
  concatenate("@key@", map(method (c)
                             iff(c = '.', '@', c)
                           end,
                           key))
end;

define method unmake-compound-key
    (compound-key :: <string>) => (key :: <string>)
  assert(compound-key.size > 5 & "@key@" = copy-sequence(compound-key, end: 5));
  map(method (c)
        iff(c = '@', '.', c)
      end,
      copy-sequence(compound-key, start: 5))
end;

/// Synopsis: Fixup the parser's 'column-number' slot based on the
///           value of the 'current-index' slot, by looking back for the
///           nearest \n or \r and taking the offset from there.  This
///           is expected to be called by parsing methods that adjust
///           current-index by means other than calling 'consume'.
define method adjust-column-number
    (p :: <coil-parser>) => (column-number :: <integer>)
  iterate loop (index = p.current-index - 1, n = 1)
    if (index < 0)
      p.column-number := n
    else
      let char = p.input-text[index];
      if (char = '\n' | char = '\r')
        p.column-number := n
      else
        loop(index - 1, n + 1)
      end
    end
  end
end method adjust-column-number;

/// Synopsis: A link to another element.  These are resolved during the
///           second pass, after the entire configuration has been parsed.
///           They're used for both keys and values.
define class <link> (<object>)
  constant slot link-name :: <string>,
    required-init-keyword: name:;
end;

define method print-object
    (link :: <link>, stream :: <stream>)
 => ()
  format(stream, "<link to %s>", link.link-name);
end;


/// Synopsis: Follow 'link' and return the value it points to.
/// If this traverses a struct that hasn't had its references expanded,
/// it expands them first.
/// Arguments:
///   link   - The <link> to follow.
///   anchor - The struct to which the link is relative, if not an
///            absolute link reference such as @root.a.b.c.
//
// TODO: Consider making struct["..a.b"] just work.  I.e., fold this into
//       element(<struct>, key).
//
// TODO: Give error when two structs @extend each other.  This pr
//
define method follow-links
    (p :: <coil-parser>, link :: <link>, anchor :: <struct>)
 => (target :: <object>)
  let keys = as(<list>, split(link.link-name, '.'));
  if (keys.head = "")
    keys := keys.tail;     // The initial dot is not used.
  end;
  iterate loop (struct = anchor, keys = keys, path = #())
    case
      ~struct =>
        parse-error(p, "Link %= tries to reference the parent of @root, "
                    "which doesn't exist.",
                    link.link-name);
      empty?(keys) =>
        struct;    // Can be any value, not necessarily a <struct>.
      ~instance?(struct, <struct>) =>
        parse-error(p, "Link %= traverses %=, which is not a struct.",
                    link.link-name,
                    join(reverse(path), "."));
      otherwise =>
        if (struct ~= anchor & struct.has-references?)
          // Must resolve references of structs through which the link
          // passes, in case the expansion adds keys.
          resolve-references!(p, struct, #());
        end;
        let key = keys.head;
        let rest = keys.tail;
        select (key by \=)
          "" =>          // We split on '.', so "..." turns into "", ""
            loop(struct.struct-parent, rest, pair(key, path));
          "@root" =>
            loop(find-root(struct), rest, pair(key, path));
          otherwise =>
            let value = element(struct, key, default: unfound());
            if (unfound?(value))
              parse-error(p, "Invalid link %=: %= is not present as a key for %=.",
                          link.link-name, key, struct);
            elseif (empty?(rest))
              value
            else
              loop(value, rest, pair(key, path))
            end;
        end select;
    end case
  end iterate
end method follow-links;

define method has-references?
    (struct :: <struct>) => (_ :: <boolean>)
  any?(rcurry(starts-with?, "@"), key-sequence(struct))
end;


/// Synopsis: Parse a link to another element in the configuration.
///           For example, "@root.foo" or "...b".
define method parse-link
    (p :: <coil-parser>) => (link :: <link>)
  eat-whitespace-and-comments(p);
  let match = regex-search($path-regex, p.input-text, start: p.current-index);
  if (match)
    let (name, _, epos) = match-group(match, 0);
    p.current-index := epos;
    adjust-column-number(p);
    if (ends-with?(name, "."))
      parse-error(p, "Links may not end with '.' since this would result "
                  "in a cycle being created.");
    end;
    make(<link>, name: name)
  else
    parse-error(p, "Link name expected");
  end
end method parse-link;

/// Synopsis: Parse a coil list, which we represent as a vector in Dylan.
///
define method parse-list
    (p :: <coil-parser>)
 => (list :: <vector>)
  let list = make(<stretchy-vector>);
  if (p.lookahead ~= '[')
    parse-error(p, "List expected");
  end;
  p.consume;  // '['
  iterate loop ()
    eat-whitespace-and-comments(p);
    if (p.lookahead ~= ']')
      add!(list, parse-any(p, key: #f));
      loop()
    end
  end;
  expect(p, "]");
  list
end method parse-list;

/// Synopsis: Parse an integer or float (digits on both sides of the '.' required)
///
define method parse-number
    (p :: <coil-parser>)
 => (number :: <number>)
  let chars = make(<stretchy-vector>);
  if (p.lookahead = '-')
    add!(chars, p.consume);
  end;
  if (~member?(p.lookahead, "0123456789"))
    parse-error(p, "Invalid number: Digit expected but got %=",
                p.lookahead);
  end;
  iterate loop ()
    let char = p.lookahead;
    if (member?(char, "0123456789"))
      p.consume;
      add!(chars, char);
      loop();
    elseif (char = '.')
      if (member?('.', chars))
        parse-error(p, "Invalid float: '.' already seen.");
      end;
      p.consume;
      add!(chars, char);
      loop();
    elseif (~char)
      #f
    elseif (~member?(char, $token-terminators))
      parse-error(p, "Invalid number: %= unexpected");
    end;
  end;
  let string = map-as(<string>, identity, chars);
  if (member?('.', string))
    string-to-float(string)
  else
    string-to-integer(string)
  end
end method parse-number;

/// Synopsis: Parse a string.  It may be a single or multi-line string.
///
define method parse-string
    (p :: <coil-parser>) => (string :: <string>)
  let char1 = p.consume;
  assert(member?(char1, "\"'"));
  let char2 = p.lookahead;
  if (char1 = char2)
    p.consume;
    let char3 = p.lookahead;
    if (char1 = char3)
      p.consume;
      parse-multi-line-string(p, char1)
    else
      ""
    end
  else
    let string = parse-simple-string(p, char1);
    if (p.lookahead ~= char1)
      parse-error(p, "Unterminated string.  Expected \"'\"");
    end;
    p.consume;
    string
  end
end method parse-string;

define table $escapes = {
    'n' => '\n',
    'r' => '\r',
    't' => '\t'
  };
    

/// Synopsis: Parse a one line string terminated by 'start-char'
///
define method parse-simple-string
    (p :: <coil-parser>, start-char :: <character>)
  let chars = make(<stretchy-vector>);
  iterate loop (escaped? = #f)
    let char = p.lookahead;
    if (escaped?)
      p.consume;
      add!(chars, element($escapes, char, default: char));
      loop(#f)
    else
      select (char)
        '\\' =>
          p.consume;
          loop(#t);
        '\n', '\r' =>
          parse-error(p, "Unterminated string");
        start-char =>
          map-as(<string>, identity, chars);   // done
        otherwise =>
          p.consume;
          add!(chars, char);
          loop(#f);
      end
    end
  end
end method parse-simple-string;
      
/// Synopsis: Parse a multi-line string terminated by 'start-char'
///
define method parse-multi-line-string
    (p :: <coil-parser>, start-char :: <character>)
  let chars = make(<stretchy-vector>);
  iterate loop (escaped? = #f)
    let char = p.consume;
    if (escaped?)
      add!(chars, element($escapes, char, default: char));
      loop(#f)
    elseif (char = '\\')
      loop(#t)
    elseif (char = start-char)
      let ch2 = lookahead(p);
      let ch3 = lookahead(p, offset: 1);
      if (char = ch2 = ch3)
        p.consume;
        p.consume;
        map-as(<string>, identity, chars)  // done
      else
        add!(chars, char);
        loop(#f)
      end
    else
      add!(chars, char);
      loop(#f)
    end
  end
end method parse-multi-line-string;


//// Pass Two
//// Resolve extension, deletion, compound keys (e.g., "x.y.z: 2"),
//// and references (e.g., "x: y.z").


/// Synopsis: Resolve @extends, @file, @delete, and @key references in 'struct'.
///
define method resolve-references!
    (p :: <coil-parser>, struct :: <struct>, seen :: <list>)
 => ()
  if (~member?(struct, seen))
    // This loop modifies 'struct', so doesn't iterate over it directly.
    for (key in slice(struct.key-sequence, 0, #f))
      let value = struct[key];
      select (key by starts-with?)
        "@extends" =>
          resolve-extends!(p, struct, value, pair(struct, seen));
          remove-key!(struct, key);
        "@file" =>
          extend(p, struct, value, seen);
          remove-key!(struct, key);
        "@key" =>
          resolve-key!(p, struct, key, value);
          remove-key!(struct, key);
          if (instance?(value, <struct>))
            resolve-references!(p, value, seen);
          end;
        "@delete" =>
          let link :: <link> = value;
          delete-key!(p, struct, link);
          remove-key!(struct, key);  // Delete the temp "@deleteX" key.
        otherwise =>
          assert(key[0] ~= '@');
          if (instance?(value, <link>))
            let link :: <link> = value;
            value := follow-links(p, link, struct);
            if (instance?(value, <struct>) & descendant?(value, struct))
              parse-error(p, "Link target %= is a descendant of containing struct.",
                          link.link-name);
            end;
          end;
          // TODO: This gets the order wrong.  It should be easy enough to add
          //       an "insert" method for <ordered-table> later on.  Generally
          //       people don't care about order for configs, so it can wait.
          struct[key] := value;
          if (instance?(value, <struct>))
            // Do not add struct to 'seen' here.
            resolve-references!(p, value, seen);
          end;
      end select;
    end for;
  end if;
end method resolve-references!;

/// Synopsis: Delete the key designated by 'link', which is relative to 'struct'.
///           Also delete the temporary @deleteX key.
///
define method delete-key!
    (p :: <coil-parser>, struct :: <struct>, link :: <link>)
 => ()
  let path = as(<list>, split(link.link-name, '.'));
  iterate loop (struct = struct, path = path, seen = #())
    if (empty?(path.tail))
      remove-key!(struct, path.head);
    else
      let child = element(struct, path.head, default: unfound());
      case
        unfound?(child) =>
          parse-error(p, "Invalid link %=: %= is not present as a key for %=.",
                      link.link-name,
                      join(reverse(seen), "."),
                      struct);
        ~instance?(child, <struct>) =>
          parse-error(p, "Invalid link %=: %= does not name a struct.",
                      link.link-name,
                      join(reverse(seen), "."));
        otherwise =>
          loop(child, path.tail, pair(path.head, seen));
      end case;
    end;
  end;
end method delete-key!;


/// Synopsis: Extend 'struct' with the values in the struct pointed to by
///           'link'.
/// Arguments:
///   seen  - A list of canonical names that have already been seen during
///           this recursion, to prevent loops.
define method resolve-extends!
    (p :: <coil-parser>, containing-struct :: <struct>, link :: <link>, seen :: <list>)
 => ()
  let target = follow-links(p, link, containing-struct);
  if (~instance?(target, <struct>))
    parse-error(p, "@extends target (%s) should be a struct but is a %s.",
                link.link-name, target.object-class);
  elseif (descendant?(target, containing-struct))
    parse-error(p, "@extends target is a descendant of containing struct.");
  elseif (member?(target, seen))
    parse-error(p, "@extends circularity: %s",
                join(reverse(pair(target, seen)), " -> ", key: struct-full-name));
  else
    extend(p, containing-struct, target, pair(target, seen));
  end;
end method resolve-extends!;


/// Synopsis: Extend 'struct' with the keys and values from 'extendee'.
///
// TODO: get order correct
define method extend
    (p :: <coil-parser>, struct :: <struct>, extendee :: <struct>, seen :: <list>)
 => ()
  for (value keyed-by key in extendee)
    if (instance?(value, <struct>))
      // Deep copy any structs in the extendee so that they can be
      // modified without affecting other places where they're extended.
      let child-struct :: <struct> = deep-copy(value);

      // Note that the parent of child-struct was set by the first pass, so
      // references are resolved relative to where it occurs in the text.
      resolve-references!(p, child-struct, seen);
    end;
    if (~key-exists?(struct, key))
      struct[key] := value;
    end;
  end;
end method extend;


/// Synopsis: Store a value for a compound key.  e.g., "a.b.c: 1"
///           Note that this creates empty structs if any intervening keys
///           don't exist.
// TODO: It might be good to have a way to warn about intervening keys
//       that don't exist, since it could be a mistake.
define method resolve-key!
    (p :: <coil-parser>, struct :: <struct>, compound-key :: <string>, value)
 => ()
  let link-name = unmake-compound-key(compound-key);
  iterate loop (struct = struct,
                path = as(<list>, split(link-name, '.')),
                seen = #())
    let simple-key = path.head;
    if (path.size = 1)
      struct[simple-key] := value;
      if (instance?(value, <struct>))
        value.struct-parent := struct;
      end;
    else
      let sub-struct = element(struct, simple-key, default: unfound());
      if (unfound?(sub-struct))
        let sub-struct = make(<struct>, name: simple-key, parent: struct);
        struct[simple-key] := sub-struct;
        loop(sub-struct, path.tail, pair(simple-key, seen));

      elseif (instance?(sub-struct, <struct>))
        loop(sub-struct, path.tail, pair(simple-key, seen));

      else
        parse-error(p, "Invalid key, %=.  %= does not name a struct.",
                    link-name,
                    join(reverse(pair(simple-key, seen)), "."));
      end;
    end;
  end;
end method resolve-key!;


// These could go in (un)common-dylan along with a slice! and slice!-setter

define method slice
    (seq :: <sequence>, bpos :: <integer>, epos :: false-or(<integer>))
 => (slice :: <sequence>)
  let len :: <integer> = seq.size;
  let _bpos = max(0, iff(bpos < 0, len + bpos, bpos));
  let _epos = iff(epos,
                  min(len, iff(epos < 0, len + epos, epos)),
                  len);
  copy-sequence(seq, start: _bpos, end: _epos)
end;

define method starts-with?
    (thing :: <object>, prefix :: <string>) => (yes? :: <boolean>)
  #f
end;

define method starts-with?
    (thing :: <string>, prefix :: <string>) => (yes? :: <boolean>)
  slice(thing, 0, prefix.size) = prefix
end;

define method ends-with?
    (thing :: <object>, suffix :: <string>) => (yes? :: <boolean>)
  #f
end;

define method ends-with?
    (thing :: <string>, suffix :: <string>) => (yes? :: <boolean>)
  slice(thing, -suffix.size, #f) = suffix
end;

// Allow negative indexes.
define method elt
    (seq :: <sequence>, index :: <integer>) => (element :: <object>)
  seq[iff(index < 0, seq.size + index, index)]
end;

