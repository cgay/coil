Module: dylan-user
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


define library coil
  use common-dylan;
  use io;
  use regular-expressions;
  use simple-parser;
  use system;
  use uncommon-dylan;

  export coil;
  export %coil;
end;

// Implementation module
define module %coil
  use common-dylan;
  use file-system;
  use regular-expressions;
  use simple-parser;
  use streams;
  use uncommon-dylan;

  // Tokenizer
  export
    <tokenizer>,
      next-token,
    <token>,
      token-type,
      token-value,
      source-filename,
      source-line,
      source-column,

    $none;
    
end module %coil;

// Interface module
define module coil
  use %coil;
  create
    parse-coil,
    write-coil;
end;


