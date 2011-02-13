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

// Interface module exports public API.
//
define module coil
  create
    $coil-major-version,
    $coil-minor-version,
    $coil-patch-version,
    parse-coil,
    <coil-error>,
    <coil-parse-error>,
    write-coil,
    $none;         // None parses to this.

  // Struct
  create
    <struct>,
    struct-name,
    struct-full-name,
    struct-parent;
end module coil;

// Implementation module exports names used by the test suite.
//
define module %coil
  use coil;
  use common-dylan,
    exclude: { format-to-string };
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
    lookahead,
    consume;

  // Links
  export
    <link>,
    link-name,
    follow-links;

  // Structs
  export
    struct-parent-setter;

end module %coil;


