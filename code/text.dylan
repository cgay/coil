Module: %coil
Synopsis: Convert coil structures to text
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.


// TODO: Could consider eliding some nesting levels, as an option.
// For example, if a struct contains a single key/value pair which is
// itself a struct...
//   a: {
//     b: 6
//   }
// they can be rewritten as a compound key:
//   a.b: 6


define open generic write-coil
    (stream :: <stream>, coil-data, #key indent) => ();

define method write-coil
    (stream :: <stream>, struct :: <struct>, #key indent = "")
 => ()
  let root? = (indent = "");
  let prefix = if (root?) "" else "{ " end;
  let suffix = if (root?) "" else "}" end;
  printing-logical-block (stream, prefix: prefix, suffix: suffix)
    for (value keyed-by key in struct,
         first? = #t then #f)
      if (~(root? & first?))
        // Don't put a blank line at the beginning of a "file".
        write(stream, "\n");
      end;
      write(stream, indent);
      write(stream, key);
      write(stream, ": ");
      write-coil(stream, value, indent: concatenate("  ", indent));
    end;
    write(stream, "\n");
    write(stream, indent, end: max(0, indent.size - 2));
  end;
end method write-coil;

define method write-coil
    (stream :: <stream>, seq :: <sequence>, #key indent = "")
 => ()
  printing-logical-block (stream, prefix: "[", suffix: "]")
    for (value in seq,
         first? = #t then #f)
      if (~first?)
        write(stream, " ");
      end;
      write-coil(stream, value, indent: concatenate("  ", indent));
    end;
  end;
end method write-coil;

define method write-coil
    (stream :: <stream>, int :: <integer>, #key indent = "")
 => ()
  write(stream, integer-to-string(int));
end;

define method write-coil
    (stream :: <stream>, float :: <float>, #key indent = "")
 => ()
  write(stream, float-to-string(float));
end;

define method write-coil
    (stream :: <stream>, string :: <string>, #key indent = "")
 => ()
  let prefix = if (member?('\n', string)
                     | member?('\r', string)
                     | (member?('"', string) & member?('\'', string)))
                 "\"\"\""
               elseif (member?('"', string))
                 "'"
               else
                 "\""
               end;
  printing-logical-block(stream, prefix: prefix, suffix: prefix)
    write(stream, string);
  end;
end method write-coil;
