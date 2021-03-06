Module:    dylan-user
Synopsis:  A configuration file format
Author:    Carl L Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


define library coil
  use collections,
    import: { table-extensions };
  use common-dylan;
  use io;
  use regular-expressions;
  use simple-parser;
  use strings;
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
  use format-out;
  use locators;
  use pprint;
  use print;
  use regular-expressions;
  use simple-parser;
  use streams;
  use strings,
    import: { ends-with?, starts-with? };
  use table-extensions,
    import: { string-hash };
  use uncommon-dylan;

  // Parser
  export
    <coil-parser>,
    parse-object,
    parse-non-struct-object,
    parse-list,
    parse-number,
    parse-string,
    lookahead,
    consume;

  // Links
  export
    <link>,
    link-name;

  // Structs
  export
    struct-parent-setter;

  // Temp. This should be moved to common-dylan.
  export
    <key-error>;

end module %coil;


