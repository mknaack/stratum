let epoch s =
  match Protocol.http_date_to_epoch s with
  | Some e -> print_string (Int64.to_string e)
  | None -> print_string "none"

let%expect_test "http_date_to_epoch: rfc1123" =
  epoch "Fri, 23 Oct 2026 20:49:00 GMT";
  [%expect {| 1792788540 |}]

let%expect_test "http_date_to_epoch: january, no weekday match required" =
  epoch "Sun, 02 Jan 2000 03:04:05 GMT";
  [%expect {| 946782245 |}]

let%expect_test "http_date_to_epoch: december" =
  epoch "Tue, 01 Dec 2026 00:00:00 GMT";
  [%expect {| 1796083200 |}]

let%expect_test "http_date_to_epoch: bad month name" =
  epoch "Fri, 23 Xyz 2026 20:49:00 GMT";
  [%expect {| none |}]

let%expect_test "http_date_to_epoch: garbage" =
  epoch "not a date";
  [%expect {| none |}]

let%expect_test "http_date_to_epoch: non-integer fields" =
  epoch "Fri, xx Oct 2026 20:49:00 GMT";
  [%expect {| none |}]
