open Rtmp

let () =
  Printf.printf "Testing AMF...\n%!";
  let open Amf in
  let test amf = assert (List.hd (decode (encode amf)) = amf) in
  test (String "a");
  test (Number 12.);
  test (Bool true);
  test (Map [("a", String "A"); ("b", Number 5.)])
