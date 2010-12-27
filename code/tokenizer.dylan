Module: %coil
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

// Tokens and tokenizers both keep track of source locations.
//
define class <source-location> (<object>)
  constant slot source-filename :: false-or(<string>) = #f,
    init-keyword: filename:;
  slot source-line :: <integer> = 0,
    init-keyword: line:;
  slot source-column :: <integer> = 0,
    init-keyword: column:;
end;

define constant $token-types :: <list>
  = #('{', '}', '[', ']', ':', '~', '=', #"path", #"value", #"eof");

define constant <token-type> = apply(one-of, $token-types);

define class <token> (<source-location>)
  constant slot token-type :: <token-type>,
    required-init-keyword: type:;
  // Some tokens such as EOF have no value.
  slot token-value :: <object> = #f,
    init-keyword: value:;
end class <token>;

// Note: Keys may start with any number of -'s but must be followed by a letter.
define constant $key-regex :: <regex> = compile-regex("-*[a-zA-Z_][\\w-]*");

define constant $path-regex :: <regex>
  = compile-regex(format-to-string("(@|\\.+)?%s(\\.%s)*",
                                   $key-regex.regex-pattern,
                                   $key-regex.regex-pattern));

define constant $float-regex :: <regex>      = compile-regex("-?[0-9]+\\.[0-9]+");
define constant $integer-regex :: <regex>    = compile-regex("-?[0-9]+");
define constant $keyword-regex :: <regex>    = compile-regex("(True|False|None)");
define constant $whitespace-regex :: <regex> = compile-regex("^(#.*|\\s+)");
define constant $line-break-regex :: <regex> = compile-regex("\r\n|\r|\n");

define class <none> (<object>)
end;
define constant $none = make(<none>);

define class <tokenizer> (<source-location>)

  // All lines are read at the outset.  We assume the config file won't be too big.
  constant slot all-lines :: <sequence>,
    required-init-keyword: lines:;

  slot line-buffer :: false-or(<string>) = #f;

  constant slot active-stack :: <deque> = make(<deque>);
end class <tokenizer>;

// Read lines from the given filename (a stream or pathname) if supplied.
define method make
    (class :: subclass(<tokenizer>), #rest args, #key filename, lines)
 => (tokenizer :: <tokenizer>)
  if (filename & ~lines)
    let lines = if(instance?(filename, <stream>))
                  split(read-to-end(filename), $line-break-regex)
                else
                  with-open-file(stream = filename, direction: #"input")
                    split(read-to-end(stream), $line-break-regex)
                  end
                end;
    apply(next-method, class, lines: lines, args)
  elseif (lines)
    // Temporarily simulate the Python convention of including the \n
    // in lines.
    apply(next-method, class, lines: map(rcurry(concatenate, "\n"), lines), args)
  else
    next-method()
  end
end method make;

define method next-line
    (tokenizer :: <tokenizer>) => (line :: <string>)
  let line-num = tokenizer.source-line;
  if (line-num >= tokenizer.all-lines.size)
    signal(make(<end-of-input-error>));
  else
    inc!(tokenizer.source-line);
    tokenizer.source-column := 1;
    tokenizer.all-lines[line-num]
  end
end method next-line;

define method peek-token
    (tokenizer :: <tokenizer>, types :: <sequence>)
 => (token :: <token>)
  let token = next-token(tokenizer, types);
  push(tokenizer.active-stack, token);
  token
end method peek-token;

define method next-token
    (tokenizer :: <tokenizer>, #rest types)
 => (token :: <token>)
  let token = %next-token(tokenizer);
  apply(expect-type, token, types);
  token
end method next-token;

define method expect-type
    (token :: <token>, #rest types) => ()
  if (~empty?(types) & ~member?(token.token-type, types))
    parse-error(token, "Expected one of these: %s",
                join(types, ", ", conjunction: "or",
                     key: curry(format-to-string, "%s")));
  end;
end;

define method make-token
    (tokenizer :: <tokenizer>, type, value)
 => (token :: <token>)
  make(<token>,
       filename: tokenizer.source-filename,
       line:   tokenizer.source-line,
       column: tokenizer.source-column,
       type:   type,
       value:  value)
end method make-token;

define method %next-token
    (tokenizer :: <tokenizer>)
 => (token :: <token>)
  block (return)
    if (~empty?(tokenizer.active-stack))
      pop(tokenizer.active-stack)
    else
      eat-whitespace-and-comments(tokenizer, return);

      // Special characters
      let char = tokenizer.line-buffer[0];
      if (member?(char, $token-types))
        let token = make-token(tokenizer, char, char);
        // This is pretty inefficient.
        // In the Dylan version it may be better to skip the next line
        // and just increment the column number.  Then later, do regex
        // searches using the start: keyword argument.  Later...
        tokenizer.line-buffer := copy-sequence(tokenizer.line-buffer, start: 1);
        inc!(tokenizer.source-column);
        return(token);
      end;

      // Note to self...  Need to check whether using text.size below is correct
      // or whether I should use group.group-end instead.

      let match = regex-search($float-regex, tokenizer.line-buffer);
      if (match)
        let (text, _, epos) = match-group(match, 0);
        let token = make-token(tokenizer, #"value", as(<double-float>, text));
        tokenizer.line-buffer := copy-sequence(tokenizer.line-buffer, start: epos);
        inc!(tokenizer.source-column, text.size);
        return(token);
      end;

      let match = regex-search($integer-regex, tokenizer.line-buffer);
      if (match)
        let (text, _, epos) = match-group(match, 0);
        let token = make-token(tokenizer, #"value", as(<integer>, text));
        tokenizer.line-buffer := copy-sequence(tokenizer.line-buffer, start: epos);
        inc!(tokenizer.source-column, text.size);
        return(token);
      end;

      let match = regex-search($keyword-regex, tokenizer.line-buffer);
      if (match)
        let (text, _, epos) = match-group(match, 0);
        let token = iff(text = "None",
                        make-token(tokenizer, #"value", $none),
                        make-token(tokenizer, #"value", text == "True"));
        tokenizer.line-buffer := copy-sequence(tokenizer.line-buffer, start: epos);
        inc!(tokenizer.source-column, text.size);
        return(token);
      end;

      let match = regex-search($path-regex, tokenizer.line-buffer);
      if (match)
        let (text, _, epos) = match-group(match, 0);
        let token = make-token(tokenizer, #"path", text);
        tokenizer.line-buffer := copy-sequence(tokenizer.line-buffer, start: epos);
        inc!(tokenizer.source-column, text.size);
        return(token);
      end;

      // Strings are special because they may span multiple lines.
      if (member?(tokenizer.line-buffer[0], #['"', '\'']))
        return(parse-string(tokenizer));
      end;

      parse-error(tokenizer, "Unrecognized input: %=", tokenizer.line-buffer);
    end;
  end block;
end method %next-token;

define method eat-whitespace-and-comments
    (tokenizer :: <tokenizer>, return :: <function>)
  iterate loop ()
    if (~tokenizer.line-buffer)
      block ()
        tokenizer.line-buffer := next-line(tokenizer);
      exception (ex :: <end-of-input-error>)
        return(make-token(tokenizer, #"eof", $none));
      end;
    end;
    // Buffer should at least have a newline.
    assert(tokenizer.line-buffer);

    // Skip over all whitespace and comments
    let match = regex-search($whitespace-regex, tokenizer.line-buffer);
    if (match)
      let (_, _, epos) = match-group(match, 0);
      tokenizer.line-buffer := copy-sequence(tokenizer.line-buffer, start: epos);
      tokenizer.source-column := epos;
      loop()
    end;
  end iterate;
end method eat-whitespace-and-comments;
    

// Strings are a bit tricky...
// The terminating quotes are optional for ''' quotes because
// they may span multiple lines. The rest of the voodoo is an
// attempt to allow escaping of quotes and require \ characters
// to always be paired with another character.
// Note: I replaced ''?(?!') below with ('|'')([^']|$) since we don't
//       yet implement (?!...).  --cgay Dec 2010
define constant $string-type-regexes
  = list(compile-regex("'''((\\.|[^\\']|('|'')([^']|$))*)(''')?"),
         compile-regex("\"\"\"((\\.|[^\\\"]|(\"|\"\")([^\"]|$))*)(\"\"\")?"),
         compile-regex("'((\\.|[^\\'])*)(')"),
         compile-regex("\"((\\.|[^\\\"])*)(\")"));

define method parse-string
    (tokenizer :: <tokenizer>)
  local method decode(buf)
          buf  // TODO: unicode
        end;
  let token = make-token(tokenizer, #"value", $none); // save start position
  let match = #f;

  // Loop until the string is terminated.
  iterate loop (regex = #f, buffer = decode(tokenizer.line-buffer))
    if (regex)
      match := regex-search(regex, buffer);
    else
      // Find the correct string type
      for (re in $string-type-regexes,
           until: match)
        match := regex-search(regex, buffer);
        if (match)
          regex := re;
        end;
      end;
    end if;

    if (~match)
      parse-error(token, "Invalid string");
    end;

    if (match-group(match, 3))
      // Done - found end of string
      token.token-value := unescape-string(match-group(match, 1));
      // Fix up the column counter
      let text = match-group(match, 0);
      // It's sad we don't have something built in to do this.
      local method rindex(text, char, #key start)
              iterate loop (i = start | text.size - 1)
                if (text[i] = char)
                  i
                elseif (i == 0)
                  #f
                else
                  loop(i - 1)
                end;
              end;
            end;
      let nl = rindex(text, '\n');
      nl & (tokenizer.source-column := text.size - nl);
      // TODO: unicode -- convert line-buffer back to string
      token
    else
      // Read another line if string has no ending ''' or """
      block ()
        buffer := concatenate(buffer, next-line(tokenizer));
      exception (ex :: <end-of-input-error>)
        parse-error(token, "Unterminated string");
      end;
      loop(regex, buffer)
    end if
  end iterate
end method parse-string;

define table $replacements = {
  '\\' => "\\",
  'n'  => "\n",
  'r'  => "\r",
  '\'' => "\'",
  '"'  => "\""
  };


define method unescape-string
    (text :: <string>) => (new :: <string>)
  // TODO: Change regex-replace to accept a function as the replacement,
  //       similar to Python's regex.sub(fn, input).  For now this is
  //       terribly inefficient.
  iterate loop (i = 0, parts = #(), prev-index = 0)
    if (i >= text.size - 1)
      join(reverse(pair(copy-sequence(text, start: prev-index), parts)), "")
    elseif (text[i] = '\\')
      let replacement = element($replacements, text[i + 1], default: #f);
      if (replacement)
        loop(i + 2,
             pair(copy-sequence(text, start: prev-index, end: i),
                  pair(replacement, parts)),
             i + 2)
      else
        loop(i + 1, parts, prev-index)
      end
    else
      loop(i + 1, parts, prev-index)
    end
  end iterate
end method unescape-string;

