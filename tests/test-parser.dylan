Module: coil-test-suite
Synopsis: Tests for the Coil parser
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define suite parser-test-suite ()
  suite basic-suite;
  suite extends-suite;
  suite file-suite;
end suite parser-test-suite;

// TODO:
// * Tests to verify that error messages are good
// 


//// basic-suite

define suite basic-suite ()
  test test-empty;
  test test-single;
  test test-many;
  test test-extends-basic;
  test test-references;
  test test-delete;
  test test-whitespace;
  test test-comments;
  test test-parse-error;
  test test-string;
  test test-list;
  test test-nested-list;
  test test-reparse;
  test parse-coil-returns-struct?;
  test compound-key-creates-sub-structs?;
end suite basic-suite;

define test test-empty ()
  let struct = parse-coil("");
  check-true("a", instance?(struct, <struct>));
  check-equal("empty input yields empty struct", struct.size, 0);
end;

define test test-single ()
  let struct = parse-coil("this: 'that'");
  write-coil(*standard-output*, struct);
  force-output(*standard-output*);
  check-equal("size = 1", struct.size, 1);
  check-equal("this = 'that'", struct["this"], "that");
end;

define test test-many ()
  let struct = parse-coil("this: 'that' int: 37 float: 2.4");
  check-equal("size = 3", struct.size, 3);

  check-instance?("this is a string", <string>, struct["this"]);
  check-equal("this = 'that'", struct["this"], "that");

  check-instance?("int is an integer", <integer>, struct["int"]);
  check-equal("int = 37", struct["int"], 37);

  check-instance?("float is a float", <float>, struct["float"]);
  check-equal("float = 2.4", struct["float"], 2.4);
end;

define test test-extends-basic ()
  let struct = parse-coil("a: {x: 7} b: { @extends: ..a }");
  check-equal("basic @extend test", struct["b.x"], 7);
end;

// Can refer to other values by path references?
define test test-references ()
  let st = parse-coil("a: { m: 5 n: { x: ...b } } "
                        "b: 6 "
                        "c: @root.a.n.x "
                        "d: a.m");
  check-equal("1", st["a.m.n.x"], 6);
  check-equal("2", st["b"], 6);
  check-equal("3", st["c"], 6);
  check-equal("4", st["d"], 5);
end;

define test test-delete ()
  let st = parse-coil("a: { m: { x: 1 y: 2 } } "
                        "b: { @extends: ..a.m ~y z: 3 }");
  check-equal("size", st["b"].size, 2);
  check-equal("has x", st["b.x"], 1);
  check-equal("has z", st["b.z"], 3);
  let unique = list(9);
  check-equal("y deleted", element(st, "b.y", default: unique), unique);
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
  check-equal("foo", struct2["x"].size, 0);
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
  for (coil in vector(
        "struct: {",
        "struct: }",
        "a: b:",
        ":",
        "[]",
        "a: ~b",
        "@x: 2",
        "x: 12c",
        "x: 12.c3",
        "x: @root",
        "x: ..a",
        "z: [{x: 2}]",            // can't have struct in list
        "z: \"lalalal \\\"",      // string is not closed
        "a: 1 z: [ =@root.a ]",
        "a: {@extends: @root.b}", // b doesn't exist
        "a: {@extends: ..b}",     // b doesn't exist
        "a: {@extends: x}",
        "a: {@extends: .}",
        "a: 1 b: { @extends: ..a }", // extend struct only
        "a: { @extends: ..a }",      // extend self
        "a: { b: {} @extends: b }",     // extend children
        "a: { b: { @extends: ...a } }", // extend parents
        "a: [1 2 3]]"
        ))
    check-condition(format-to-string("%= gets parse error", coil),
                    <coil-parse-error>,
                    parse-coil(coil));
  end;
end test test-parse-error;

// We don't support =a, which this seems to test.
//define test test-order ()
//end;

define test test-string ()
  for (item in #(#("'x'", "x"),
                 #("\"x\"", "x"),
                 #("'''x'''", "x"),
                 #("\"\"\"x\"\"\"", "x")))
    let (input, expected) = apply(values, item);
    let struct-rep = format-to-string("k: %s", input);
    let struct-exp = make(<struct>);
    struct-exp["k"] := expected;
    check-equal(format-to-string("%s => %=", input, expected),
                parse-coil(struct-rep), struct-exp);
  end;
end test test-string;

define test test-list ()
  let struct = parse-coil("x: ['a' 1 2.0 True False None]");
  check-equal("list contents", struct["x"], vector("a", 1, 2.0d0, #t, #f, $none));
end;

define test test-nested-list ()
  let struct = parse-coil("x: ['a' ['b' 'c']]");
  check-equal("nested list", struct["x"], #["a", #["b", "c"]]);
end;

define test test-reparse ()
  let text = "a: '''this\nis\r\na\tstring\n\r\n\t'''";
  let coil = parse-coil(text);
  let new = parse-coil(with-output-to-string(s)
                         write-coil(s, coil)
                       end);
  check-equal("reparsed same as orig?", coil, new);
end test test-reparse;


//// @extends

define suite extends-suite ()
  test test-extend-and-delete;
  test test-extends-references;
  test test-extends-2;
  test test-relative-paths;
  test test-extend-copies;
end suite extends-suite;

define method get-test-struct ()
  parse-coil("A: {\n"
             "    a: 'a'\n"
             "    b: 'b'\n"
             "    c: 'c'\n"
             "}\n"
             "B: {\n"
             "    @extends: ..A\n"
             "    e: [ 'one' 2 'omg three' ]\n"
             "    ~c\n"
             "}\n"
             "C: {\n"
             "    a: ..A.a\n"
             "    b: @root.B.b\n"
             "}\n"
             "D: {\n"
             "    @extends: @root.B\n"
             "}\n"
             "\n"
             "E: {\n"
             "    F.G.H: {\n"
             "        a: 1 b: 2 c: 3\n"
             "    }\n"
             "\n"
             "    F.G.I: {\n"
             "        @extends: ..H\n"
             "    }\n"
             "}\n"
             )
end method get-test-struct;

define test test-extend-and-delete ()
  let tree = get-test-struct();
  check-equal("aaa", tree["B.a"], "a");
  check-equal("bbb", tree["B.b"], "b");
  check-condition("ccc", <key-error>, tree["B.c"]);
  check-equal("ddd", tree["B.e"], #["one", 2, "omg three"]);
  check-equal("size", tree["B"].size, 3);
end;

define test test-extends-references ()
  let tree = get-test-struct();
  check-equal("aaa", tree["C.a"], "a");
  check-equal("bbb", tree["C.b"], "b");
  check-equal("size", tree["C"].size, 2);
end;

define test test-extends-2 ()
  let tree = get-test-struct();
  check-equal("aaa", tree["D.a"], "a");
  check-equal("bbb", tree["D.b"], "b");
  check-condition("ccc", <key-error>, tree["D.c"]);
  check-equal("ddd", tree["D.e"], #["one", 2, "omg three"]);
  check-equal("size", tree["D"].size, 3);
end;

define test test-relative-paths ()
  let tree = get-test-struct();
  check-equal("aaa", tree["E.F.G.H.a"], 1);
  check-equal("bbb", tree["E.F.G.I.a"], 1);
  check-equal("ccc", tree["E.F.G.H"], tree["E.F.G.I"]);
end;

define test test-double-extend ()
  let text = "a: { aa: 1 cc: 3} b: { bb: 2 cc: 4 } c: { @extends: ..a @extends: ..b }";
  let root = parse-coil(text);
  check-equal("aaa", root.size, 3);
  check-equal("bbb", root["c.aa"], 1);
  check-equal("ccc", root["c.bb"], 2);
  check-equal("ddd", root["c.cc"], 3);  // first @extend to set cc wins
end test test-double-extend;

/// Synopsis: Verify that when a struct is extended it is deep copied so that
///           multiple extensions can be modified independently.
define test test-extend-copies ()
  let text = "a: { aa: { aaa: 1 aab: 2 } }\n"
             "b: { @extends: ..a aa.aaa: 3 ~aa.aab }\n"
             "c: { @extends: ..a aa.aaa: 4 aa.aab: 5 }\n";
  let root = parse-coil(text);
  check-equal("one-a", root["b.aa"].size, 1);
  check-equal("one-b", root["b.aa.aaa"], 3);

  check-equal("two-a", root["c.aa"].size, 2);
  check-equal("two-b", root["c.aa.aaa"], 4);
  check-equal("two-c", root["c.aa.aab"], 5);
end;

define test parse-coil-returns-struct? ()
  let st = parse-coil("a: 5");
  check-true("a", instance?(st, <struct>));
  check-equal("b", st.size, 1);
  check-equal("c", st["a"], 5);
  check-condition("top-level must be struct", <coil-parse-error>, parse-coil("123"));
end;

define test compound-key-creates-sub-structs? ()  
  let st = parse-coil("a.b: 3");
  check-equal("a", st.size, 1);
  check-equal("b", st["a"]["b"], 3);
end;




//// @file

// Because we have no way to associate data files with tests yet...

define constant $file-content
  = list(list("example.coil",
              "# sample file used by tests\n"
                "x: 1\n"
                "y: { a: 2  x: ..x  a2: @root.y.a }\n"),
         list("example2.coil",
              "# another file used in tests\n"
                "sub: {@file: \"example.coil\"\n"
                "      x: \"foo\"\n"
                "      y.a: \"bar\"\n"
                "     }\n"
                "sub2: {@file: \"example.coil\"}\n"),
         list("example3.coil",
              "@file: \"example.coil\"\n"
                "y.b: 3\n"),
         list("complex.coil",
              "sub: {\n"
                "    y: 2\n"
                "    x: \"default\"\n"
                "    two: {\n"
                "        parentx: ..x\n"
                "        value: \"hello\"\n"
                "        zzz: 8\n"
                "    }\n"
                "}\n"),
         list("filesubimport.coil",
              "# import file\n"
                "imp: {@file: \"complex.coil\"}\n"
                "\n"
                "# import sub-struct from file\n"
                "sub: {@file: [\"complex.coil\" \"sub\"]\n"
                "      x: \"foo\"\n"
                "     }\n"
                "\n"
                "# import sub-sub-struct from file\n"
                "subsub: {@file: ['complex.coil' 'sub.two']\n"
                "         value: 'override'\n"
                "         ~zzz}\n"
                "x: 'bar'\n"));

define function write-test-coil-files
    () => ()
  for (item in $file-content)
    let (filename, content) = apply(values, item);
    fs/with-open-file(out = make-test-locator(filename),
                      direction: #"output",
                      if-exists: #"overwrite")
      write(out, content)
    end;
  end;
end function write-test-coil-files;

define method make-test-locator
    (filename :: <string>) => (locator :: <locator>)
  merge-locators(as(<file-locator>, filename), fs/temp-directory())
end;


define suite file-suite (setup-function: write-test-coil-files)
  test test-file-load;
  test test-file-inclusion;
  test test-file-inclusion-at-root;
  test test-import-file-sub-struct;
end;

define test test-file-load ()
  let root = parse-coil(make-test-locator("example.coil"));
  check-equal("aaa", root["x"], 1);
  check-equal("bbb", root["y.a"], 2);
  check-equal("ccc", root["y.x"], 1);
  check-equal("ddd", root["y.a2"], 2);
end;

define test test-file-inclusion ()
  let root = parse-coil(make-test-locator("example2.coil"));
  check-equal("aaa", root["sub.x"], "foo");
  check-equal("bbb", root["sub.y.a"], "bar");
  check-equal("ccc", root["sub.y.x"], "foo");
  check-equal("ddd", root["sub.y.a2"], "bar");
  check-equal("ggg", root["sub2.y.a"], 2);
  check-equal("hhh", root["sub2.y.x"], 1);
  check-equal("iii", root["sub2.y.a2"], 2);
end;

define test test-file-inclusion-at-root ()
  let root = parse-coil(make-test-locator("example3.coil"));
  check-equal("x = 1", root["x"], 1);
  check-equal("y.a = 2", root["y.a"], 2);
  check-equal("y.x = 1", root["y.x"], 1);
  check-equal("y.a2 = 2", root["y.a2"], 2);
  check-equal("y.b = 3", root["y.b"], 3);
end;

define test test-import-file-sub-struct ()
  let root = parse-coil(make-test-locator("filesubimport.coil"));
  check-equal("a", root["imp.sub.two.value"], "hello");
  check-equal("b", root["sub.x"], "foo");
  check-equal("c", root["subsub.parentx"], "default");
  check-equal("d", root["subsub.value"], "override");

  let unique = list(9);
  check-equal("delete element inherited from file",
              element(root, "subsub.zzz", default: unique),
              unique);
end;
