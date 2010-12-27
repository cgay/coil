Module: %coil
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define class <coil-error> (<format-string-condition>, <error>)
end;

define class <coil-parse-error> (<coil-error>)
end;

define method parse-error
    (tok :: <source-location>, format-string :: <string>, #rest args)
  if (tok.source-filename)
    error(make(<coil-parse-error>,
               format-string: concatenate("<%s:%s> ", format-string),
               format-arguments: concatenate(list(tok.source-filename,
                                                  tok.source-line),
                                             args)))
  else
    error(make(<coil-parse-error>,
               format-string: format-string,
               format-arguments: args))
  end;
end method parse-error;

define class <end-of-input-error> (<coil-error>)
end;

