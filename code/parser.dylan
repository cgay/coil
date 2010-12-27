Module: %coil
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

/// Synopsis: All coil errors are subclasses of this.
define class <coil-error> (<format-string-condition>, <error>)
end;


define class <coil-parse-error> (<coil-error>)
end;


define class <coil-parser> (<object>)
  // Source is for error reporting only.  It could be a file name, a stream, etc.
  slot input-source :: <object>, required-init-keyword: source:;
  // Text is the entire original coil source text.
  slot input-text :: <string> = "";
  // Index points to the next character to be read by next-char.
  slot current-index :: <integer> = 0;
  // Line and column are for error reporting.  They are maintained by next-char.
  slot line-number :: <integer> = 0, init-keyword: line:;
  slot column-number :: <integer> = 0, init-keyword: column:;
end class <coil-parser>;


/// Synopsis: Signal <coil-parse-error> with the given 'format-string' and
///           'args' as the message.  If the current source location is known
///           it is prefixed to the message.
define method parse-error
    (parser :: <coil-parser>, format-string, #rest args)
  let context = iff(parser.line-number = -1,
                    "",
                    format-to-string("<%d:%d> ",
                                     parser.line-number, parser.column-number));
  error(make(<coil-parse-error>,
             format-string: concatenate(context, format-string),
             format-arguments: args));
end method parse-error;


/// Synopsis: Parse coil formatted text from the given 'source'.
///           This is the main user-visible entry point for parsing.
define open generic parse-coil
    (source :: <object>) => (coil :: <ordered-string-table>);

define method parse-coil
    (source :: <locator>) => (coil :: <ordered-string-table>)
  with-open-file (stream = source, direction: input:)
    parse-struct(make(<coil-parser>,
                      source: as(<string>, source),
                      text: read-to-end(stream)))
  end
end method parse-coil;

define method parse-coil
    (source :: <stream>) => (coil :: <ordered-string-table>)
  parse-struct(make(<coil-parser>,
                    source: source,
                    text: read-to-end(stream)))
end method parse-coil;


/// Synopsis: parse and return any valid coil object.  A struct, list,
///     integer, float, string, boolean, or None.  This is used for
///     parsing list elements and struct values, for example.
define method parse-coil
    (parser :: <coil-parser>)
 => (object :: <object>)
  eat-whitespace-and-comments(parser);
  let char = peek-char(parser);
  if (char = '"' | char = '\'')
    next-char(parser);
    parse-string(parser, char)
  elseif (char = '{')
    next-char(parser);
    parse-struct(parser)
  elseif (char = '[')
    next-char(parser);
    parse-list(parser)
  elseif (member?(char, "0123456789"))
    parse-number(parser)
  elseif (char = 'T')
    expect(parser, "True");
    #t
  elseif (char = 'F')
    expect(parser, "False");
    #f
  elseif (char = 'N')
    expect(parser, "None");
    $none
  else
    parse-error(parser, "Unexpected input starting with %c.", char)
  end
end method parse-coil;

/// Synopsis: Return the next unread input character, or #f if at end.
define method peek-char
    (parser :: <coil-parser>, #key offset :: <integer> = 0)
 => (char :: false-or(<character>))
  let text = parser.input-text;
  let idx = parser.current-index;
  if (idx + index >= text.size)
    #f
  else
    text[idx + offset]
  end
end method peek-char;

/// Synopsis: Consume and return the next unread input character.  If at
///           end-of-input signal <coil-parse-error>.
define method next-char
    (parser :: <coil-parser>)
 => (char :: false-or(<character>))
  let char = peek-char(parser);
  if (char)
    inc!(parser.current-index);
    char
  else
    parse-error("End of coil text encountered.");
  end;
end method next-char;


/// Synopsis: Parse a struct.  The opening '{' has already been eaten.
///     This is where parsing begins for a new file.
define method parse-struct
    (parser :: <coil-parser>)
 => (struct :: <ordered-string-table>)
  let struct = make(<ordered-string-table>);
  iterate loop ()
    eat-whitespace-and-comments(parser);
    let char = peek(parser);
    if (char = '}')
      next-char(parser);
      struct
    elseif (member?(char, $key-start-charset))
      let key = parse-key(parser);
      if (key[0] = '@')
        select (key by \=)
          "@extends" =>
            let super = parse-path(parser);
            extend(struct, super);
          "@file" =>
            let filename = parse-coil(parser);
            if (instance?(filename, <string>))
              let super = parse-coil(filename);
              extend(struct, super);
            else
              parse-error("Expected a filename for @file but got %=", filename);
            end;
          otherwise =>
            parse-error("Unrecognized struct key syntax: %=", key);
        end select;
        loop();
      else
        struct[key] := value;
        loop();
      end if;
    else
      parse-error(parser, "Invalid struct key starting with %c.", char);
    end if;
  end iterate;
  struct
end method parse-struct;

/// Synopsis: Parse a coil list, which we represent as a vector in Dylan.
///     The opening '[' has already been eaten.
define method parse-list
    (parser :: <coil-parser>)
 => (list :: <vector>)
  let list = make(<stretchy-vector>);
  iterate loop ()
    eat-whitespace-and-comments(parser);
    if (peek-char(parser) ~= ']')
      // TODO: This allows structs inside lists but the Python version doesn't.
      //       This will mess with relative path references.
      add!(list, parse-coil(parser));
      loop()
    end
  end;
  expect(parser, ']');
  list
end method parse-list;

/// Synopsis: Parse an integer or float (digits on both sides of the '.' reqired)
define method parse-number
    (parser :: <coil-parser>)
 => (number :: <number>)
  let chars = make(<stretchy-vector>);
  iterate loop ()
    let char = peek-char(parser);
    if (member?(char, ".0123456789"))
      next-char(parser);
      add!(chars, char);
      loop()
    end;
  end;
  let string = make(<string>, size: chars.size);
  map-into!(string, chars);
  if (member?('.', string))
    string-to-float(string)
  else
    string-to-integer(string)
  end
end method parse-number;

/// Synopsis: Parse a string.  It may be a single or multi-line string.
///     The 'start-char' has already been eaten.
///
/// Arguments:
///     start-char - The character that started the string.  If this is a
///                  multi-line string then there will be two more of these
///                  to eat before reading the actual string.  The end
///                  must match this character.
define method parse-string
    (parser :: <coil-parser>, start-char :: one-of('"', '\''))
 => (string :: <string>)
  let char2 = next-char(parser);
  let char3 = peek-char(parser, index: 1);
  let multi-line? = (start-char = char2 = char3);
  if (multi-line?)
    next-char(parser);
    next-char(parser);
  end;
  let chars = make(<stretchy-vector>);
  iterate loop (escaped? = #f)
    let char = next-char(parser);
    if (char = escape-char)
      if (escaped?)
        add!(chars, char);
        loop(#f);
      else
        loop(#t);
      end;
    elseif (char = start-char)
      if (multi-line?)
        let ch2 = next-char(parser);
        if (ch2 = start-char)
          let ch3 = next-char(parser);
          if (ch3 ~= start-char)
            add!(chars, start-char);
            add!(chars, start-char);
            loop(#f);
          end;
        else
          add!(chars, start-char);
          loop(#f);
        end;
      end;
    end if;
  end iterate;
  let string = make(<string>, size: chars.size);
  map-into!(string, chars);
  string
end method parse-string;
