Module: %coil
Synopsis: Ad-hoc recursive descent parser for coil configs
Author: Carl Gay
Copyright: Copyright (c) 2011 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

// TODO: track struct parents

define constant $whitespace :: <string>
  = " \t\n\r";

define constant $token-terminators :: <string>
  = concatenate($whitespace, "}]\"");

define constant $key-regex :: <regex>
  = compile-regex("-*[a-zA-Z_][\\w-]*");

define constant $path-regex :: <regex>
  = compile-regex(format-to-string("(@|\\.+)?%s(\\.%s)*",
                                   $key-regex.regex-pattern,
                                   $key-regex.regex-pattern));

/// Synopsis: All coil errors are subclasses of this.
define class <coil-error> (<format-string-condition>, <error>)
end;


define class <coil-parse-error> (<coil-error>)
end;


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


/// Synopsis: Signal <coil-parse-error> with the given 'format-string' and
///           'args' as the message.  If the current source location is known
///           it is prefixed to the message.
define method parse-error
    (p :: <coil-parser>, format-string, #rest args)
  let context = format-to-string("<%d:%d> ", p.line-number, p.column-number);
  error(make(<coil-parse-error>,
             format-string: concatenate(context, format-string),
             format-arguments: args));
end;


/// Synopsis: Parse coil formatted text from the given 'source'.
///           This is the main user-visible entry point for parsing.
define open generic parse-coil
    (source :: <object>) => (coil :: <struct>);

define method parse-coil
    (source :: <locator>) => (coil :: <struct>)
  with-open-file (stream = source, direction: input:)
    parse-struct-attributes(make(<coil-parser>,
                                 source: as(<string>, source),
                                 text: read-to-end(stream)),
                            make(<struct>))
  end
end;

define method parse-coil
    (source :: <stream>) => (coil :: <struct>)
  parse-struct-attributes(make(<coil-parser>,
                               source: source,
                               text: read-to-end(source)),
                          make(<struct>))
end;

define method parse-coil
    (source :: <string>) => (coil :: <struct>)
  parse-struct-attributes(make(<coil-parser>,
                               source: #f,
                               text: source),
                          make(<struct>))
end;

/// Synopsis: Parse a struct.  The opening '{' has already been eaten.
///           This is where parsing begins for a new file.
define method parse-coil
    (p :: <coil-parser>) => (struct :: <struct>)
  let struct = make(<struct>);
  eat-whitespace-and-comments(p);
  let first-char = p.lookahead;
  select (first-char)
    #f =>
      #f;  // empty string (at top level only) yields empty struct.
    '{' =>
      p.consume;
      parse-struct-attributes(p, struct);
      expect(p, "}");
    otherwise =>
      parse-error(p, "Invalid struct.  Expected '{' or EOF.");
  end;
  struct
end method parse-coil;

/// Synopsis: Parse the attributes of a struct and add them to the 'struct'
///           argument passed in.
define method parse-struct-attributes
    (p :: <coil-parser>, struct :: <struct>) => (struct :: <struct>)
  iterate loop ()
    eat-whitespace-and-comments(p);
    select (p.lookahead)
      '}', #f =>
        #f;  // done
      '~' =>
        p.consume;
        let path = parse-path(p);
        todo-deletion;
        loop();
      '@' =>
        p.consume;
        select (p.lookahead)
          'e' =>
            expect(p, "extends:");
            // TODO: Need to find out whether these can be forward references
            //       or not.  If yes, then insert the reference into the struct
            //       in order with a unique key and resolve it later.
            parse-path(p);
            todo-extend;
          'f' =>
            expect(p, "file:");
            let filename = parse-any(p);
            if (instance?(filename, <string>))
              map-into(struct, identity, parse-coil(filename));
            else
              parse-error(p, "Expected a filename for @file but got %=",
                          filename);
            end;
          otherwise =>
            parse-error(p, "Unrecognized special attribute");
        end select;
        loop();
      otherwise =>
        let key = parse-key(p);
        eat-whitespace(p);
        expect(p, ":");
        eat-whitespace(p);
        struct[key] := parse-any(p);
        loop();
    end;
  end iterate;
  struct
end method parse-struct-attributes;


/// Synopsis: parse and return any valid coil object.  A struct, list,
///           integer, float, string, boolean, or None.  This is used for
///           parsing list elements and struct values, for example.
///           Note that this may only be called when an ENTIRE OBJECT is
///           expected.  That specifically excludes parsing a path.
///
define method parse-any
    (p :: <coil-parser>)
 => (object :: <object>)
  eat-whitespace-and-comments(p);
  let char = p.lookahead;
  select (char by member?)
    "'\"" =>
      parse-string(p);
    "{" =>
      parse-coil(p);
    "[" =>
      parse-list(p);
    "-0123456789" =>
      parse-number(p);
    "T" =>
      expect(p, "True");
      #t;
    "F" =>
      expect(p, "False");
      #f;
    "N" =>
      expect(p, "None");
      $none;
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
    (p :: <coil-parser>, string :: <string>) => ()
  let start = p.current-index;
  for (char in string)
    if (char ~= p.consume)
      parse-error(p, "Expected %= but got %=",
                  string,
                  copy-sequence(p.input-text, start: start, end: p.current-index));
    end;
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

// Note: Keys may start with any number of -'s but must be followed by a
//       letter.
define method parse-key
    (p :: <coil-parser>) => (key :: <string>)
  let match = regex-search($key-regex, p.input-text,
                           start: p.current-index);
  if (match)
    let (key, _, epos) = match-group(match, 0);
    p.current-index := epos;
    key
  else
    parse-error(p, "Struct key expected");
  end
end method parse-key;

/// Synopsis: Parse the reference starting with the next token and delete it
///           from the given struct.
// TODO: Document that deletions can only reference something 
//       inside the struct currently being parsed.
define method parse-deletion
    (p :: <coil-parser>)
  todo-deletion
end;

/// Synopsis: A <reference> which will be resolved during a second pass, after the
///           entire configuration has been parsed.
define class <reference> (<object>)
  constant slot reference-path :: <string>,
    required-init-keyword: path:;
end;

/// Synopsis: Parse a reference to another element in the configuration.
///           For example, "@root.foo" or "...b".
define method parse-path
    (p :: <coil-parser>) => (ref :: <reference>)
  eat-whitespace-and-comments(p);
  let match = regex-search($path-regex, p.input-text, start: p.current-index);
  if (match)
    let (path, _, epos) = match-group(match, 0);
    p.current-index := epos;
    make(<reference>, path: path)
  else
    parse-error(p, "Reference path expected");
  end
end method parse-path;

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
      // TODO: This allows structs inside lists but the Python version doesn't.
      //       This will mess with relative path references.
      add!(list, parse-any(p));
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

