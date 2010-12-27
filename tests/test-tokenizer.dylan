Module: coil-test-suite
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define suite tokenizer-test-suite ()
  test test-empty;
  test test-path;
  test test-string;
  test test-unicode;
  test test-numbers;
  test test-boolean;
  test test-none;
  test test-counters;
  test test-special-characters;
end suite tokenizer-test-suite;

define test test-empty ()
  let tokenizer = make(<tokenizer>, lines: #[""]);
  check-equal("no input yields EOF token",
              tokenizer.next-token.token-type, #"eof");
end test test-empty;

define test test-path ()
  let tokenizer = make(<tokenizer>, lines: #["somekey"]);
  let first = tokenizer.next-token;
  check-instance?("first is a <token>", <token>, first);
  check-equal("token type is PATH", #"path", first.token-type);
  check-equal("token value is 'somekey'", "somekey", first.token-value);
  check-equal("token line is 1", 1, first.source-line);
  check-equal("token column is 1", 1, first.source-column);
  check-equal("next token is EOF", #"eof", tokenizer.next-token.token-type);
end test test-path;

define test test-string ()
  let tokenizer = make(<tokenizer>, lines: #["'string'"]);
  let first = tokenizer.next-token;
  check-equal("token type is VALUE", #"value", first.token-type);
  check-instance?("token value is a <string>", <string>, tokenizer.token-value);
  check-equal("value is 'string'", "string", tokenizer.token-value);
  check-equal("token line is 1", 1, first.source-line);
  check-equal("token column is 1", 1, first.source-column);
  check-equal("next token is EOF", #"eof", tokenizer.next-token.token-type);
end test test-string;

define test test-unicode ()
/*
  let tok = make(<tokenizer>,
                 lines: #["'\<3456>'"],
                 encoding: #"utf-8");
  let first = tok.next-token;
  check-equal("token type is VALUE", #"value", first.token-type);
  check-instance?("token value is a <string>", <unicode-string>, tok.token-value);
  check-equal("value is '\<3456>'", "string", tok.token-value)
  check-equal("token line is 1", 1, first.source-line);
  check-equal("token column is 1", 1, first.source-column);
  check-equal("next token is EOF", #"eof", first.next-token.token-type);
*/
end test test-unicode;

define test test-numbers ()
  let tokenizer = make(<tokenizer>, lines: #["1 2.0 -3 -4.0 0"]);
  for (value in #[1, 2.0, -3, -4.0, 0])
    let token = tokenizer.next-token;
    check-equal("token type is VALUE", #"value", token.token-type);
    check-equal(fmt("token value is %s", value),
                value, token.token-value);
    check-instance?(fmt("token is a %s", value.object-class),
                    value.object-class, token.token-value);
  end;
  check-equal("next token is EOF", #"eof", tokenizer.next-token.token-type);
end test test-numbers;

define test test-boolean ()
  let tokenizer = make(<tokenizer>, lines: #["True False"]);
  let token = tokenizer.next-token;
  check-equal("token type is VALUE", #"value", token.token-type);
  check-instance?("token is a <boolean>", <boolean>, token.token-value);
  check-equal("token value is #t", #t, token.token-value);

  let token = tokenizer.next-token;
  check-equal("token type is VALUE (2)", #"value", token.token-type);
  check-instance?("token is a <boolean> (2)", <boolean>, token.token-value);
  check-equal("token value is #f", #f, token.token-value);
end test test-boolean;

define test test-none ()
  let tokenizer = make(<tokenizer>, lines: #["None"]);
  let token = tokenizer.next-token;
  check-equal("token type is VALUE", #"value", token.token-type);
  check-equal("token value is $none", $none, token.token-value);
  check-equal("next token is EOF", #"eof", tokenizer.next-token.token-type);
end test test-none;

define test test-counters ()
  let tokenizer = make(<tokenizer>,
                       lines: #["'string' '''foo''' '' '''''' other",
                                "'''multi line string",
                                "it is crazy''' hi",
                                "  bye"]);
  tokenizer.next-token;
  let token = tokenizer.next-token;
  check-equal("2nd token line is 1", 1, token.source-line);
  check-equal("2nd token column is 10", 10, token.source-column);

  let token = tokenizer.next-token;
  check-equal("3rd token line is 1", 1, token.source-line);
  check-equal("3rd token column is 20", 20, token.source-column);

  let token = tokenizer.next-token;
  check-equal("4th token line is 1", 1, token.source-line);
  check-equal("4th token column is 23", 23, token.source-column);

  let token = tokenizer.next-token;
  check-equal("5th token line is 1", 1, token.source-line);
  check-equal("5th token column is 30", 30, token.source-column);

  let token = tokenizer.next-token;
  check-equal("6th token line is 2", 2, token.source-line);
  check-equal("6th token column is 1", 1, token.source-column);

  let token = tokenizer.next-token;
  check-equal("7th token line is 3", 3, token.source-line);
  check-equal("7th token column is 16", 16, token.source-column);

  let token = tokenizer.next-token;
  check-equal("8th token line is 4", 4, token.source-line);
  check-equal("8th token column is 3", 3, token.source-column);

  check-equal("next token is EOF", #"eof", tokenizer.next-token.token-type);
end test test-counters;

define test test-special-characters ()
  let line = "{}[]:~=";
  let tokenizer = make(<tokenizer>, lines: list(line));
  for (char in line)
    check-equal(fmt("char %c is type %s", char, char),
                char, tokenizer.next-token.token-type);
  end;
  check-equal("next token is EOF", #"eof", tokenizer.next-token.token-type);
end test test-special-characters;

