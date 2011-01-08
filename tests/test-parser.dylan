Module: coil-test-suite
Synopsis: Tests for the Coil parser
Author: Carl Gay
Copyright: Copyright (c) 2010 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define suite parser-test-suite ()
  suite basics-test-suite;
  suite extends-test-suite;
  suite file-test-suite;
  suite map-test-suite;
end suite parser-test-suite;

//// Basics

define suite basics-test-suite ()
  test test-empty;
  test test-single;
  test test-many;
  test test-struct;
  test test-extends-1;
  test test-references;
  test test-delete;
  test test-file;
  test test-file-sub;
  test test-file-delete;
  test test-file-expansion;
  //test test-package;     // @package is Python-specific
  test test-whitespace;
  test test-comments;
  test test-parse-error;
  test test-order;
  test test-list;
  test test-nested-list;
  test test-reparse;
end suite basics-test-suite;

define test test-empty ()
  let struct = parse-coil("");
  check-equal("empty string yields empty struct?", struct.size, 0);
end;

define test test-single ()
  let struct = parse-coil("this: 'that'");
  check-equal("one attribute yields struct of size 1?", struct.size, 1);
  check-equal("this = 'that'", struct["this"], "that");
end;

define test test-many ()
  let struct = parse-coil("this: 'that' int: 1 float: 2.0");
  check-equal("length = 3", struct.size, 3);

  check-instance?("this is a string", <string>, struct["this"]);
  check-equal("this = 'that'", struct["this"], "that");

  check-instance?("int is an integer", <integer>, struct["int"]);
  check-equal("int = 1", struct["int"], 1);

  check-instance?("float is a float", <float>, struct["float"]);
  check-equal("float = 2.0", struct["float"], 2.0);
end;

define test test-struct ()
  let struct = parse-coil("foo: { bar: 'baz' } -moo: 'cow'");
  check-equal("[foo][bar] = baz", struct["foo"]["bar"], "baz");
  check-equal("foo.bar = baz", struct["foo.bar"], "baz");
  check-equal("@root.foo.bar = baz", struct["@root.foo.bar"], "baz");
  check-equal("-moo = cow", struct["-moo"], "cow");
end;

define test test-extends-1 ()
end;

define test test-references ()
end;

define test test-delete ()
end;

define test test-file ()
end;

define test test-file-sub ()
end;

define test test-file-delete ()
end;

define test test-file-expansion ()
end;

define test test-whitespace ()
  // Whitespace around keys and values in a struct.
  let struct1 = parse-coil("x :1 y : 2 z\t:3");
  check-equal("x = 1", struct1["x"], 1);
  check-equal("y = 2", struct1["y"], 2);
  check-equal("z = 3", struct1["z"], 3);

  // Whitespace around Struct delimiters
  let struct2 = parse-coil("x:{} y:{z:9} z: {\n a: 1\n}\n");
  for (key in #["x", "y", "z"])
    check-instance?("blah", <struct>, struct2[key]);
  end;
  check-equal("foo", struct2.size, 0);
  check-equal("bar", struct2["y.z"], 9);
  check-equal("baz", struct2["z.a"], 1);

  let struct3 = parse-coil("x: [ 1   2 3\t4\r\n5]");
  check-equal("whitespace in list", struct3["x"], #[1, 2, 3, 4, 5]);
end;

define test test-comments ()
  let struct = parse-coil("y: [12 #hello\n]");
  check-equal("y = #[12]", struct["y"], #[12]);
end;

define test test-parse-error ()
end;

define test test-order ()
end;

define test test-list ()
  let struct = parse-coil("x: ['a' 1 2.0 True False None]");
  check-equal("list contents", struct["x"], vector("a", 1, 2.0d0, #t, #f, $none));
end;

define test test-nested-list ()
  let struct = parse-coil("x: ['a' ['b' 'c']]");
  check-equal("nested list", struct["x"], #["a", #["b", "c"]]);
end;

define test test-reparse ()
  let text = "a: 'this\nis\r\na\tstring\n\r\n\t'";
  let coil = parse-coil(text);
  let new = parse-coil(with-output-to-string(s)
                         write-coil(s, coil)
                       end);
  check-equal("reparsed same as orig?", coil, new);
end test test-reparse;


//// @extends

define suite extends-test-suite ()
  test test-extends-basic;
  test test-extends-and-delete;
  test test-extends-references;
  test test-extends-2;
  test test-relative-paths;
end suite extends-test-suite;

define test test-extends-basic ()
end;

define test test-extends-and-delete ()
end;

define test test-extends-references ()
end;

define test test-extends-2 ()
end;

define test test-relative-paths ()
end;


//// @file

define suite file-test-suite ()
  test test-file-1;
  test test-file-2;
  test test-file-3;
end;

define test test-file-1 ()
end;

define test test-file-2 ()
end;

define test test-file-3 ()
end;


//// @map

define suite map-test-suite ()
  test test-map;
  test test-map-extends;
end suite map-test-suite;

define test test-map ()
end;

define test test-map-extends ()
end;
