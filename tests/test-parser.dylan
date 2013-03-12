Module: coil-test-suite
Synopsis: Tests for the Coil parser
Author: Carl Gay
Copyright: Copyright (c) 2013 Carl L Gay.  All rights reserved.
License:   See LICENSE.txt in this distribution for details.

define suite parser-test-suite ()
  suite basics-test-suite;
  suite extends-test-suite;
  suite file-test-suite;
end suite parser-test-suite;

// TODO:
// * Tests to verify that error messages are good
// 


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
  //test test-order;       // Deprecated =a not supported.
  test test-list;
  test test-nested-list;
  test test-reparse;
  test test-follow-links;
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
  let struct = parse-coil("a: {x: 'x'} b: { @extends: ..a }");
  check-equal("basic @extend test", struct["b.x"], "x");
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

define test test-follow-links ()
  let parser = make(<coil-parser>, source: "", text: "");
  let root = make(<struct>, name: "@root");
  let child = make(<struct>, name: "child");
  let grandchild = make(<struct>, name: "grandchild");

  // parent links
  child.struct-parent := root;
  grandchild.struct-parent := child;
  
  // child links
  root["child"] := child;
  child["grandchild"] := grandchild;

  for (item in list(list("..child", child, child),
                    list("...child", grandchild, child),
                    list("...child.grandchild", grandchild, grandchild),
                    list("@root", root, root),
                    list("@root.child", root, child),
                    list("@root.child.grandchild", root, grandchild)))
    let (link-name, anchor, expected) = apply(values, item);
    let actual = follow-links(parser, make(<link>, name: link-name), anchor);
    check-equal(format-to-string("%= resolves correctly", link-name),
                expected,
                actual);
  end;
end test test-follow-links;


//// @extends

define suite extends-test-suite ()
  test test-extends-basic;
  test test-extends-and-delete;
  test test-extends-references;
  test test-extends-2;
  test test-relative-paths;
  test test-extend-copies;
end suite extends-test-suite;

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

/*

A: {
    B: {
        C: "arf"
    }
    D: "abc"
}

E: {
    @extends: @root.A
    B.C: "other"
}
# => E: { B: { C: "other" } D: "abc" }

E: {
    B.C: "other"
    @extends: @root.A
}
# => E: { B: { C: "arf" } D: "abc" }
# i.e., E.B is completely replaced by (a copy of) A.B

*/

define test test-extends-basic ()
  let tree = get-test-struct();
  check-equal("aaa", tree["A.a"], "a");
  check-equal("bbb", tree["A.b"], "b");
  check-equal("ccc", tree["A.c"], "c");
  check-equal("size", tree["A"].size, 3);
end;

define test test-extends-and-delete ()
  let tree = get-test-struct();
  check-equal("aaa", tree["B.a"], "a");
  check-equal("bbb", tree["B.b"], "b");
  check-condition("ccc", <invalid-key-error>, tree["B.c"]);
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
  check-condition("ccc", <invalid-key-error>, tree["D.c"]);
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

//// 


//// @file

define method make-test-locator
    (filename :: <string>) => (locator :: <locator>)
  // TODO: This is temporary (obviously).  Need a way to associate
  // data files with test projects.
  merge-locators(as(<file-locator>, filename),
                 as(<directory-locator>, "/home/cgay/dylan/src/coil/"))
end;


define suite file-test-suite ()
  test test-file-1;
  test test-file-2;
  test test-file-3;
end;

define test test-file-1 ()
  let root = parse-coil(make-test-locator("tests/example.coil"));
  check-equal("aaa", root["x"], 1);
  check-equal("bbb", root["y.a"], 2);
  check-equal("ccc", root["y.x"], 1);
  check-equal("ddd", root["y.a2"], 2);
  check-equal("eee", root["y.x2"], 1);
  check-equal("fff", root["y.x3"], "1");
end;

define test test-file-2 ()
  let root = parse-coil(make-test-locator("tests/example2.coil"));
  check-equal("aaa", root["sub.x"], "foo");
  check-equal("bbb", root["sub.y.a"], "bar");
  check-equal("ccc", root["sub.y.x"], "foo");
  check-equal("ddd", root["sub.y.a2"], "bar");
  check-equal("eee", root["sub.y.x2"], "foo");
  check-equal("fff", root["sub.y.x3"], "foo");
  check-equal("ggg", root["sub2.y.a"], 2);
  check-equal("hhh", root["sub2.y.x"], 1);
  check-equal("iii", root["sub2.y.a2"], 2);
  check-equal("jjj", root["sub2.y.x2"], 1);
  check-equal("kkk", root["sub2.y.x3"], "1");
  check-equal("lll", root["sub3.y.a"], "bar");
  check-equal("mmm", root["sub3.y.x"], "zoink");
  check-equal("nnn", root["sub3.y.a2"], "bar");
  check-equal("ooo", root["sub3.y.x2"], "zoink");
  check-equal("ppp", root["sub3.y.x3"], "zoink");
end;

define test test-file-3 ()
  let root = parse-coil(make-test-locator("tests/example3.coil"));
  check-equal("aaa", root["x"], 1);
  check-equal("bbb", root["y.a"], 2);
  check-equal("ccc", root["y.x"], 1);
  check-equal("ddd", root["y.a2"], 2);
  check-equal("eee", root["y.b"], 3);
end;


//// @map

/* map is not supported.

define suite map-test-suite ()
  test test-map;
  test test-map-extends;
end suite map-test-suite;

define method get-map-struct ()
  parse-coil("expanded: {\n"
             "    a1: {\n"
             "        z: 1\n"
             "        x: 1\n"
             "        y: 1\n"
             "    }\n"
             "    a2: {\n"
             "        z: 1\n"
             "        x: 2\n"
             "        y: 3\n"
             "    }\n"
             "    a3: {\n"
             "        z: 1\n"
             "        x: 3\n"
             "        y: 5\n"
             "    }\n"
             "    b1: {\n"
             "        z: 2\n"
             "        x: 1\n"
             "        y: 1\n"
             "    }\n"
             "    b2: {\n"
             "        z: 2\n"
             "        x: 2\n"
             "        y: 3\n"
             "    }\n"
             "    b3: {\n"
             "        z: 2\n"
             "        x: 3\n"
             "        y: 5\n"
             "    }\n"
             "}\n"
             "map: {\n"
             "    @map: [1 2 3]\n"
             "    x: [1 2 3]\n"
             "    y: [1 3 5]\n"
             "    a: { z: 1 }\n"
             "    b: { z: 2 }\n"
             "}\n"
             "map1: {\n"
             "    @extends: ..map\n"
             "}\n"
             "map2: {\n"
             "    @extends: ..map\n"
             "    a: { z: 3 }\n"
             "    j: [7 8 9]\n"
             "}\n"
               )
end method get-map-struct;

define test test-map ()
  let tree = get-map-struct();
  check-equal("map = expanded", tree["map"], tree["expanded"]);
end;

define test test-map-extends ()
  let tree = get-map-struct();
  check-equal("aaa", tree["map1"], tree["expanded"]);
  check-equal("bbb", tree["map2.a1.z"], 3);
  check-equal("ccc", tree["map2.a1.j"], 7);
  check-equal("ddd", tree["map2.a2.z"], 3);
  check-equal("eee", tree["map2.a2.j"], 8);
  check-equal("fff", tree["map2.a3.z"], 3);
  check-equal("ggg", tree["map2.a3.j"], 9);
end;

*/
