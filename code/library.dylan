Module: dylan-user
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


define library coil
  use collections,
    import: { table-extensions };
  use common-dylan;
  use io;
  use regular-expressions;
  use simple-parser;
  use string-extensions;
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
  use locators;
  use regular-expressions;
  use simple-parser;
  use streams;
  use string-hacking,
    import: { <case-insensitive-character-set> };
  use table-extensions,
    import: { string-hash, <hash-state> };
  use uncommon-dylan;

  // Parser
  export
    <coil-parser>,
    parse-any,
    parse-inheritance,
    parse-list,
    parse-number,
    parse-string,
    parse-struct,
    next-char,
    peek-char;
end module %coil;


