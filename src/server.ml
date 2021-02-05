(** Test server which dumps recieved data. *)

let () =
  let dump = FLV.open_out "dump.flv" in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 1935));
  Unix.listen socket 5;
  while true do
    Printf.printf "Waiting for client\n%!";
    let s, caller = Unix.accept socket in
    let cnx = RTMP.create_connection s in
    let handler ~timestamp ~stream = function
      | `Audio data ->
        FLV.write_audio dump timestamp data
      | `Video data ->
        FLV.write_video dump timestamp data
      | `Command (_, `Delete_stream _) ->
        FLV.close_out dump
      | `Data [AMF.String l; AMF.String "onMetaData"; AMF.Map m] ->
        Printf.printf "matadata (%s): %s\n%!" l (AMF.to_string (AMF.Map m));
        FLV.write_metadata dump m
      | `Data amf ->
        Printf.printf "data: %s\n%!" (AMF.list_to_string amf)
      | `Command (tid, `Connect) ->
        Printf.printf "Connecting...\n%!";
        RTMP.window_acknowledgement_size cnx 500000;
        RTMP.set_peer_bandwidth cnx 500000 `Dynamic;
        RTMP.set_chunk_size cnx cnx.chunk_size;
        RTMP.command cnx "_result" tid [AMF.Object ["fmsVer", AMF.String "FMS/3,0,1,123"; "capabilities", AMF.Number 31.]];
      | `Command (tid, `Create_stream) ->
        Printf.printf "Creating stream...\n%!";
        (* TODO: increment counter *)
        let stream_id = 0 in
        RTMP.command cnx "_result" tid [AMF.Null; AMF.Number (float_of_int stream_id)];
      | `Command (tid, `Publish (name, kind)) ->
        RTMP.command cnx "onStatus" tid [AMF.Null; AMF.Object ["level", AMF.String "status"; "code", AMF.String "NetStream.Publish.Start"; "description", AMF.String ("Publishing stream " ^ name)]];
      | `Command (_, `Result amf) ->
        Printf.printf "result\n%!"
      | `Set_chunk_size n ->
        RTMP.set_chunk_size cnx n
      | `Command (_, `Unhandled (name, amf)) ->
        Printf.printf "Unhandled command: %s\n%!" name
      | _ -> assert false
    in
    Printf.printf "Accepting connection!\n%!";
    RTMP.handshake cnx;
    (* Let's go. *)
    while true do
      RTMP.read_chunk cnx;
      RTMP.handle_messages cnx handler
    done;
    Printf.printf "Done with connection\n%!";
    ignore (exit 0)
  done
