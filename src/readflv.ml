let () =
  let f = FLV.open_in "test.flv" in
  while true do
    match snd (FLV.read_tag f) with
    | `Audio data -> Printf.printf "audio (%d bytes)\n\n%!" (String.length data)
    | `Video data -> Printf.printf "video (%d bytes)\n\n%!" (String.length data)
    | `Data (n, v) -> Printf.printf "data %s: %s\n\n%!" n (AMF.to_string v)
  done
