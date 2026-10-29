open Query

let compile sql ?(params = []) ?(fetch = 101) () =
  let q = Compile.parse sql in
  print_string (Compile.compile ~params ~fetch ~offset:0 q)

let%expect_test "select * " =
  compile "SELECT * FROM c" ();
  [%expect
    {| SELECT doc FROM docs WHERE db = ? AND coll = ? ORDER BY lsn LIMIT 101 OFFSET 0 |}]

let%expect_test "simple where" =
  compile "SELECT * FROM c WHERE c.name = 'foo' AND c.age > 5" ();
  [%expect
    {| SELECT doc FROM docs WHERE db = ? AND coll = ? AND ((doc #> '{"name"}' = '"foo"'::jsonb) AND (doc #> '{"age"}' > '5'::jsonb)) ORDER BY lsn LIMIT 101 OFFSET 0 |}]

let%expect_test "projections" =
  compile "SELECT c.name AS n, c.city FROM c" ();
  [%expect
    {| SELECT jsonb_build_object('n', doc #> '{"name"}', 'city', doc #> '{"city"}') FROM docs WHERE db = ? AND coll = ? ORDER BY lsn LIMIT 101 OFFSET 0 |}]

let%expect_test "value + params + order by" =
  compile "SELECT VALUE c.id FROM c WHERE c.age = @age ORDER BY c.name DESC"
    ~params:[ ("age", "5") ]
    ();
  [%expect
    {| SELECT doc #> '{"id"}' FROM docs WHERE db = ? AND coll = ? AND (doc #> '{"age"}' = '5'::jsonb) ORDER BY doc #> '{"name"}' DESC, lsn LIMIT 101 OFFSET 0 |}]

let%expect_test "in + between + is_defined" =
  compile
    "SELECT * FROM c WHERE c.a IN ('x', 'y') OR (c.b BETWEEN 1 AND 3 AND \
     IS_DEFINED(c.c))"
    ();
  [%expect
    {| SELECT doc FROM docs WHERE db = ? AND coll = ? AND ((doc #> '{"a"}' = ANY (ARRAY['"x"'::jsonb, '"y"'::jsonb])) OR ((doc #> '{"b"}' >= '1'::jsonb AND doc #> '{"b"}' <= '3'::jsonb) AND jsonb_exists(doc, '$.c'))) ORDER BY lsn LIMIT 101 OFFSET 0 |}]

let%expect_test "string funcs + array_contains + top/offset" =
  compile
    "SELECT * FROM c WHERE CONTAINS(c.n, 'oo') AND ARRAY_CONTAINS(c.tags, 'a') \
     OFFSET 2 LIMIT 5"
    ();
  [%expect
    {| SELECT doc FROM docs WHERE db = ? AND coll = ? AND ((jsonb_typeof(doc #> '{"n"}') = 'string' AND strpos(doc #>> '{"n"}', 'oo') > 0) AND (doc #> '{"tags"}' @> '["a"]'::jsonb)) ORDER BY lsn LIMIT 5 OFFSET 2 |}]

let%expect_test "from alias" =
  compile "SELECT f.x FROM Families f WHERE f.y = true" ();
  [%expect
    {| SELECT jsonb_build_object('x', doc #> '{"x"}') FROM docs WHERE db = ? AND coll = ? AND (doc #> '{"y"}' = 'true'::jsonb) ORDER BY lsn LIMIT 101 OFFSET 0 |}]

let%expect_test "parse error" =
  (try ignore (Compile.parse "SELECT FROM")
   with Compile.Error e -> print_string e);
  [%expect {| parse error at character 7 near 'FROM' |}]

let%expect_test "patch_set: set + incr + remove" =
  let ops =
    [
      { Patch.op = Patch.Set; path = "/city"; value = Some (`String "CPH") };
      { Patch.op = Patch.Incr; path = "/n"; value = Some (`Int 2) };
      { Patch.op = Patch.Remove; path = "/old"; value = None };
    ]
  in
  print_string (Patch.patch_set ops);
  [%expect
    {| (jsonb_set(jsonb_set(doc, '{"city"}', '"CPH"'::jsonb, true), '{"n"}', to_jsonb(COALESCE((jsonb_set(doc, '{"city"}', '"CPH"'::jsonb, true) #>> '{"n"}')::numeric, 0) + 2::numeric)) #- '{"old"}') |}]

let%expect_test "compile_where: patch condition" =
  print_string (Patch.compile_where "from c where c.n > 5 and IS_DEFINED(c.x)");
  [%expect {| ((doc #> '{"n"}' > '5'::jsonb) AND jsonb_exists(doc, '$.x')) |}]
