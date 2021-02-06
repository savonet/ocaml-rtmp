open Rtmp

let () =
  let f = Flv.open_in "test.flv" in
  while true do
    match snd (Flv.read_tag f) with
      | `Audio data ->
          Printf.printf "audio (%d bytes)\n\n%!" (String.length data)
      | `Video data ->
          Printf.printf "video (%d bytes)\n\n%!" (String.length data)
      | `Data (n, v) -> Printf.printf "data %s: %s\n\n%!" n (Amf.to_string v)
  done
