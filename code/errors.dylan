Module:    %coil
Author:    Carl L Gay
Copyright: Copyright (c) 2011 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


/// Synopsis: All coil errors are subclasses of this.
///
define open class <coil-error> (<format-string-condition>, <error>)
end;


// TODO: move this to common-dylan or thereabouts, and fix up the exception
//       hierarchy in general.
define open class <invalid-key-error> (<format-string-condition>, <error>)
end;

