(** Test server which dumps recieved data. *)

open Rtmp

let () =
  let once = ref false in
  Arg.parse
    ["--once", Arg.Set once, "Only accept one client and then exit."]
    (fun _ -> ())
    "server [options]";
  let dump = Flv.open_out "dump.flv" in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 1935));
  Unix.listen socket 5;
  while true do
    Printf.printf "Waiting for client\n%!";
    let s, _ = Unix.accept socket in
    let cnx = Rtmp.create_connection s in
    let handler ~timestamp ~stream:_ = function
      | `Audio data -> Flv.write_audio dump timestamp data
      | `Video data -> Flv.write_video dump timestamp data
      | `Command (_, `Delete_stream _) -> Flv.close_out dump
      | `Data [Amf.String l; Amf.String "onMetaData"; Amf.Map m] ->
          Printf.printf "matadata (%s): %s\n%!" l (Amf.to_string (Amf.Map m));
          Flv.write_metadata dump m
      | `Data amf -> Printf.printf "data: %s\n%!" (Amf.list_to_string amf)
      | `Command (tid, `Connect) ->
          Printf.printf "Connecting...\n%!";
          Rtmp.window_acknowledgement_size cnx 500000;
          Rtmp.set_peer_bandwidth cnx 500000 `Dynamic;
          Rtmp.set_chunk_size cnx cnx.chunk_size;
          Rtmp.command cnx "_result" tid
            [
              Amf.Object
                [
                  ("fmsVer", Amf.String "FMS/3,0,1,123");
                  ("capabilities", Amf.Number 31.);
                ];
            ]
      | `Command (tid, `Create_stream) ->
          Printf.printf "Creating stream...\n%!";
          (* TODO: increment counter *)
          let stream_id = 0 in
          Rtmp.command cnx "_result" tid
            [Amf.Null; Amf.Number (float_of_int stream_id)]
      | `Command (tid, `Publish (name, _)) ->
          Rtmp.command cnx "onStatus" tid
            [
              Amf.Null;
              Amf.Object
                [
                  ("level", Amf.String "status");
                  ("code", Amf.String "NetStream.Publish.Start");
                  ("description", Amf.String ("Publishing stream " ^ name));
                ];
            ]
      | `Command (_, `Result _) -> Printf.printf "result\n%!"
      | `Set_chunk_size _ -> ()
      | `Command (_, `Unhandled (name, _)) ->
          Printf.printf "Unhandled command: %s\n%!" name
      | _ -> assert false
    in
    Printf.printf "Accepting connection!\n%!";
    Rtmp.handshake cnx;
    (* Let's go. *)
    try
      while true do
        Rtmp.read_chunk cnx;
        Rtmp.handle_messages cnx handler
      done;
    with Rtmp.Connection_closed ->
      Printf.printf "Done with connection\n%!";
      if !once then exit 0;
  done
