Module: %coil
Synopsis: Ad-hoc recursive descent parser for coil configs
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/*

This parser makes two passes.  During the first pass a tree of
<struct-prototype>s which track inherited and deleted values is built.
The second pass creates a copy of that tree using real <struct>
objects. that don't involve any links outside the struct are parsed,
and most struct keys are filled in.  The second pass resolves
@extends, @file, deletions, compound struct keys (i.e., a.b.c: foo)
and struct value links (i.e., foo: a.b.c).  This is necessary because
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
  = compile-regex(format-to-string("(@|\\.+)?%s", $extended-key-regex.regex-pattern));


/// Synopsis: '$none' is what "None" parses to.
///
define class <none> (<object>) end;
define constant $none :: <none> = make(<none>);

define class <coil-parser> (<object>)
  constant slot source-locator :: false-or(<file-locator>) = #f,
    init-keyword: source:;

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
    (parser :: <coil-parser>, format-string, #rest args)
  let context = format-to-string("@%d:%d ", parser.line-number, parser.column-number);
  let message = concatenate(context,
                            apply(format-to-string, format-string, args),
                            "\n", parser.current-line,
                            "\n", parser.indicator-line);
  error(make(<coil-parse-error>, format-string: message));
end;

/// Synopsis: Return the line pointed to by 'current-index'.
///
define method current-line
    (parser :: <coil-parser>) => (line :: <string>)
  let max = parser.input-text.size;
  let curr = parser.current-index;
  let epos = min(position(parser.input-text, '\n', start: curr) | max,
                 position(parser.input-text, '\r', start: curr) | max);
  slice(parser.input-text,
        parser.current-index - parser.column-number + 1,
        epos)
end;

/// Synopsis: Return a line that indicates which character 'current-index'
///           points to.  ".........^"
define method indicator-line
    (parser :: <coil-parser>) => (line :: <string>)
  let line = make(<string>, size: parser.column-number, fill: '.');
  line[parser.column-number - 1] := '^';
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
                     source: source,
                     text: read-to-end(stream)))
  end
end;

define method parse-coil
    (source :: <stream>) => (coil :: <struct>)
  %parse-coil(make(<coil-parser>, text: read-to-end(source)))
end;

define method parse-coil
    (source :: <string>) => (coil :: <struct>)
  %parse-coil(make(<coil-parser>, text: source))
end;

define method %parse-coil
    (parser :: <coil-parser>) => (struct :: <struct>)
  let root = make(<struct-prototype>, name: "@root");
  parse-struct-attributes(parser, root);
  // Copy the <struct-prototype> into an actual <struct>, resolving
  // all references.
  let struct = make(<struct>, name: "@root");
  copy-struct-into(root, struct);
  struct
end method %parse-coil;

define method parse-struct
    (parser :: <coil-parser>, key :: <string>, parent :: <struct>)
 => (struct :: <struct>)
  format-out("parse-struct(%=, %=)\n", key, parent);
  let new-struct = make(<struct-prototype>, name: key, parent: parent);
  eat-whitespace-and-comments(parser);
  select (parser.lookahead)
    '{' =>
      parser.consume;
      parse-struct-attributes(parser, new-struct);
      expect(parser, "}");
    otherwise =>
      parse-error(parser, "Invalid struct.  Expected '{'.");
  end;
  new-struct
end method parse-struct;

/// Synopsis: Parse the attributes of a struct and add them to the 'struct'
///           argument passed in.
define method parse-struct-attributes
    (parser :: <coil-parser>, struct :: <struct-prototype>)
 => (struct :: <struct-prototype>)
  format-out("parse-struct-attributes(%=)\n", struct);
  iterate loop ()
    eat-whitespace-and-comments(parser);
    select (parser.lookahead)
      '}', #f =>
        #f;  // done
      '~' =>
        parser.consume;
        let path :: <string> = parse-path(parser);
        remove-key!(struct, path);
        loop();
      '@' =>
        parser.consume;
        parse-special-key(parser, struct);
        loop();
      otherwise =>
        let key-or-path = parse-key(parser, struct);
        expect(parser, "", ":", "");
        let elements = split(key-or-path, '.');
        let key = elements[elements.size - 1];
        let value = parse-object(parser, key, struct);
        format-out("parse-struct-attributes: %s[%=] := %=\n", struct, key-or-path, value);
        struct[key-or-path] := value;
        loop();
    end;
  end iterate;
  struct
end method parse-struct-attributes;

// Parse @extends or @file.  The '@' has already been consumed.
define method parse-special-key
    (parser :: <coil-parser>, struct :: <struct-prototype>) => ()
  format-out("parse-special-key(%=)\n", struct);
  select (parser.lookahead)
    'e' =>
      expect(parser, "extends", ":");
      let path = parse-path(parser);
      format-out("processing '@extends: %s', struct = %s\n", path, struct);
      let source = struct[path];
      if (~instance?(source, <struct>))
        parse-error(parser, "Target of @extends reference is not a struct.");
      else
        extend-struct(parser, struct, source);
      end;
    'f' =>
      expect(parser, "file", ":");
      parse-file(parser, struct);
    'm' =>
      expect(parser, "map", ":");
      parse-error(parser, "@map is not supported.");
    otherwise =>
      parse-error(parser, "Unrecognized special attribute");
  end select
end method parse-special-key;

// Extend struct with the values from source.
define method extend-struct
    (parser :: <coil-parser>, struct :: <struct-prototype>, source :: <struct>) => ()
  format-out("extend-struct(%=, %=)\n", struct, source);
  for (key in source.struct-order)
    add-new!(struct.secondary-order, key);
    struct.secondary-values[key] := source[key];
  end;
end method extend-struct;

// Synopsis: Parse "@file: <spec>" where <spec> is a pathname string or
//           a list of [ "pathname" path.to.struct ] and incorporate the
//           target struct's values.  The "@file:" has already been consumed.
define method parse-file
    (parser :: <coil-parser>, struct :: <struct-prototype>) => ()
  local method fail ()
          parse-error(parser, "Target of @file keyword must be a filename or a "
                        "list of the form [ 'filename' 'path.to.struct' ].");
        end;
  // Passing allow-struct?: #t here because locally we can give a
  // better error message.
  let target = parse-object(parser, "@root", struct);
  let path = "@root";
  select (target by instance?)
    <string> => #f;
    <sequence> =>
      if (target.size ~= 2
            | ~every?(rcurry(instance?, <string>), target))
        fail();
      else
        path := target[1];
        target := target[0];
      end;
    otherwise => fail();
  end;
  // TODO(cgay): This should really merge against the current
  // pathname, not the original pathname passed to the parser.  i.e.,
  // nested @file links won't use the correct relative path.
  // TODO(cgay): catch circular @file references.
  let file = as(<file-locator>, target);
  if (parser.source-locator)
    file := merge-locators(file, parser.source-locator);
  end;
  let file-struct = parse-coil(file);
  let source = file-struct[path];
  if (~instance?(source, <struct>))
    parse-error(parser, "Target of @file reference is not a struct.");
  else
    extend-struct(parser, struct, source);
  end
end method parse-file;

define method parse-non-struct-object
    (parser :: <coil-parser>) => (value :: <object>)
  parse-object(parser, #f, #f)
end;

/// Synopsis: parse and return any valid coil object.  A struct, list,
///           integer, float, string, boolean, or None.  This is used for
///           parsing list elements and struct values, for example.
///           Note that this may only be called when an ENTIRE OBJECT is
///           expected, which specifically excludes parsing a path.
/// Arguments:
///   parser - Coil parser.
///   for-key - The key for which this value is being parsed.  This is used
///         to give a name to the struct created, if this method parses
///         a struct.  #f indicates that this is not called in a struct
///         context
///
define method parse-object
    (parser :: <coil-parser>, key :: false-or(<string>), parent :: false-or(<struct>))
 => (object :: <object>)
  eat-whitespace-and-comments(parser);
  let char = parser.lookahead;
  select (char by member?)
    "'\"" =>
      parse-string(parser);
    "{" =>
      if (~parent)
        parse-error(parser, "Token '{' unexpected. Trying put a struct inside a list?");
      end;
      parse-struct(parser, key, parent);
    "[" =>
      parse-list(parser);
    "-0123456789" =>
      parse-number(parser);
    // TODO: These three aren't right because they could be TrueXXX etc.
    //       That's one reason a real tokenizer separate from the parser
    //       would be better.
    "T" =>
      expect(parser, "True");
      #t;
    "F" =>
      expect(parser, "False");
      #f;
    "N" =>
      expect(parser, "None");
      $none;
    ".@" =>
      // TODO(cgay): fix this to return the target of the path
      parse-path(parser);
    otherwise =>
      parse-error(parser, "Unexpected input starting with %=.", char);
  end select
end method parse-object;

/// Synopsis: Return the next unread input character, or #f if at end.
define method lookahead
    (parser :: <coil-parser>, #key offset :: <integer> = 0)
 => (char :: false-or(<character>))
  let text = parser.input-text;
  let idx = parser.current-index;
  if (idx + offset >= text.size)
    #f
  else
    text[idx + offset]
  end
end method lookahead;

/// Synopsis: Consume and return the next unread input character.  If at
///           end-of-input signal <coil-parse-error>.
define method consume
    (parser :: <coil-parser>) => (char :: false-or(<character>))
  let char = parser.lookahead;
  if (char)
    inc!(parser.current-index);
    if (char = '\n')
      inc!(parser.line-number);
      parser.column-number := 1;
    else
      inc!(parser.column-number);
    end;
    char
  else
    parse-error(parser, "End of coil text encountered.");
  end;
end method consume;

define method expect
    (parser :: <coil-parser>, #rest strings :: <string>) => ()
  for (string in strings)
    let start = parser.current-index;
    for (char in string)
      if (char = parser.lookahead)
        parser.consume
      else
        parse-error(parser, "Expected %= but got %=",
                    string, slice(parser.input-text, start, parser.current-index));
      end;
    end;
    eat-whitespace(parser);
  end;
end method expect;

define method eat-whitespace-and-comments
    (parser :: <coil-parser>) => ()
  iterate loop ()
    if (parser.lookahead = '#')
      eat-comment(parser);
      loop()
    elseif (member?(parser.lookahead, $whitespace))
      parser.consume;
      loop()
    end;
  end;
end;

/// Synopsis: Consume until not looking at a whitespace char.
///
define method eat-whitespace
    (parser :: <coil-parser>) => ()
  while (member?(parser.lookahead, $whitespace))
    parser.consume;
  end;
end;

/// Synopsis: Consume a comment that starts with '#' and ends with '\n'.
///
define method eat-comment
    (parser :: <coil-parser>) => ()
  while (parser.consume ~= '\n')
  end;
end;

/// Synopsis: Parse a struct key, which may be a simple identifier such
///           as "a" or a compound identifier such as "..a.b.c".
define method parse-key
    (parser :: <coil-parser>, parent :: <struct>)
 => (key :: <string>)
  let match = regex-search($extended-key-regex, parser.input-text,
                           start: parser.current-index);
  if (match)
    let (key, _, epos) = match-group(match, 0);
    parser.current-index := epos;
    adjust-column-number(parser);
    key
  else
    parse-error(parser, "Struct key expected");
  end
end method parse-key;

/// Synopsis: Fixup the parser's 'column-number' slot based on the
///           value of the 'current-index' slot, by looking back for the
///           nearest \n or \r and taking the offset from there.  This
///           is expected to be called by parsing methods that adjust
///           current-index by means other than calling 'consume'.
define method adjust-column-number
    (parser :: <coil-parser>) => (column-number :: <integer>)
  iterate loop (index = parser.current-index - 1, n = 1)
    if (index < 0)
      parser.column-number := n
    else
      let char = parser.input-text[index];
      if (char = '\n' | char = '\r')
        parser.column-number := n
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


/// Synopsis: Parse a path to another element in the configuration.
///           For example, "@root.foo" or "...b".
define method parse-path
    (parser :: <coil-parser>) => (path :: <string>)
  eat-whitespace-and-comments(parser);
  let match = regex-search($path-regex, parser.input-text,
                           start: parser.current-index);
  if (match)
    let (path, _, epos) = match-group(match, 0);
    parser.current-index := epos;
    adjust-column-number(parser);
    path
  else
    parse-error(parser, "Path expected");
  end
end method parse-path;

/// Synopsis: Parse a coil list, which we represent as a vector in Dylan.
///
define method parse-list
    (parser :: <coil-parser>) => (list :: <vector>)
  let list = make(<stretchy-vector>);
  if (parser.lookahead ~= '[')
    parse-error(parser, "List expected");
  end;
  parser.consume;  // '['
  iterate loop ()
    eat-whitespace-and-comments(parser);
    if (parser.lookahead ~= ']')
      add!(list, parse-non-struct-object(parser));
      loop()
    end
  end;
  expect(parser, "]");
  list
end method parse-list;

/// Synopsis: Parse an integer or float (digits on both sides of the '.' required)
///
define method parse-number
    (parser :: <coil-parser>) => (number :: <number>)
  let chars = make(<stretchy-vector>);
  if (parser.lookahead = '-')
    add!(chars, parser.consume);
  end;
  if (~member?(parser.lookahead, "0123456789"))
    parse-error(parser, "Invalid number: Digit expected but got %=",
                parser.lookahead);
  end;
  iterate loop ()
    let char = parser.lookahead;
    if (member?(char, "0123456789"))
      parser.consume;
      add!(chars, char);
      loop();
    elseif (char = '.')
      if (member?('.', chars))
        parse-error(parser, "Invalid float: '.' already seen.");
      end;
      parser.consume;
      add!(chars, char);
      loop();
    elseif (~char)
      #f
    elseif (~member?(char, $token-terminators))
      parse-error(parser, "Invalid number: %= unexpected");
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
    (parser :: <coil-parser>) => (string :: <string>)
  let char1 = parser.consume;
  assert(member?(char1, "\"'"));
  let char2 = parser.lookahead;
  if (char1 = char2)
    parser.consume;
    let char3 = parser.lookahead;
    if (char1 = char3)
      parser.consume;
      parse-multi-line-string(parser, char1)
    else
      ""
    end
  else
    let string = parse-simple-string(parser, char1);
    if (parser.lookahead ~= char1)
      parse-error(parser, "Unterminated string.  Expected \"'\"");
    end;
    parser.consume;
    string
  end
end method parse-string;

define table $escapes = {
    'n' => '\n',
    'r' => '\r',
    't' => '\t'
  };
    

/// Synopsis: Parse a one line string terminated by 'start-char'
///           The start token (' or ") has already been consumed.
///
define method parse-simple-string
    (parser :: <coil-parser>, start-char :: <character>) => (_ :: <string>)
  let chars = make(<stretchy-vector>);
  iterate loop (escaped? = #f)
    let char = parser.lookahead;
    if (escaped?)
      parser.consume;
      add!(chars, element($escapes, char, default: char));
      loop(#f)
    else
      select (char)
        '\\' =>
          parser.consume;
          loop(#t);
        '\n', '\r' =>
          parse-error(parser, "Unterminated string");
        start-char =>
          map-as(<string>, identity, chars);   // done
        otherwise =>
          parser.consume;
          add!(chars, char);
          loop(#f);
      end
    end
  end
end method parse-simple-string;
      
/// Synopsis: Parse a multi-line string terminated by start-char.
///           The start token (''' or """) has already been consumed.
///
define method parse-multi-line-string
    (parser :: <coil-parser>, start-char :: <character>) => (_ :: <string>)
  let chars = make(<stretchy-vector>);
  iterate loop (escaped? = #f)
    let char = parser.consume;
    if (escaped?)
      add!(chars, element($escapes, char, default: char));
      loop(#f)
    elseif (char = '\\')
      loop(#t)
    elseif (char = start-char)
      let ch2 = lookahead(parser);
      let ch3 = lookahead(parser, offset: 1);
      if (char = ch2 & char = ch3)
        parser.consume;
        parser.consume;
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
