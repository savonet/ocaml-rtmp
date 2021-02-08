open Rtmp

let () =
  let i = Flv.open_in "test.flv" in
  let o = Flv.open_out "test-copy.flv" in
  try
    while true do
      match Flv.read_tag i with
      | t, `Audio data ->
        Printf.printf "audio (%d bytes)\n\n%!" (String.length data);
        Flv.write_audio o t data
      | t, `Video data ->
        Printf.printf "video (%d bytes)\n\n%!" (String.length data);
        Flv.write_video o t data
      | _, `Data (n, v) ->
        Printf.printf "data %s: %s\n\n%!" n (Amf.to_string v);
        if n = "onMetaData" then
          let m = Amf.get_map v in
          Flv.write_metadata o m
    done
  with
  | End_of_file -> Flv.close_out o
