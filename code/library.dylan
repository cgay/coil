Module:    dylan-user
Synopsis:  A powerful configuration library
Author:    Carl L Gay
Copyright: Copyright (c) 2011 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


define library coil
  use collections,
    import: { table-extensions };
  use common-dylan;
  use io;
  use regular-expressions;
  use simple-parser;
  use system;
  use uncommon-dylan;

  export coil;
  export %coil;
end library coil;

// Interface module
define module coil
  create
    parse-coil,
    <coil-error>,
    <coil-parse-error>,
    write-coil;

  // Struct
  create
    <struct>;
end module coil;

// Implementation module
define module %coil
  use coil;
  use common-dylan;
  use file-system;
  use format;
  use locators;
  use pprint;
  use regular-expressions;
  use simple-parser;
  use streams;
  use table-extensions,
    import: { string-hash };
  use uncommon-dylan;

  // Parser
  export
    <coil-parser>,
    parse-any,
    parse-list,
    parse-number,
    parse-string,
    parse-struct,
    lookahead,
    consume;
end module %coil;


