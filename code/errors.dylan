Module:    %coil
Author:    Carl L Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


/// Synopsis: All coil errors are subclasses of this.
///
define open class <coil-error> (<format-string-condition>, <error>)
end;


/// Synopsis: Error finding/setting a struct key.
define open class <key-error> (<format-string-condition>, <error>)
end;

