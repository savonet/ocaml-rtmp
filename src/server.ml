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
      | `Video data ->
        FLV.write_video dump (RTMP.now cnx) data;
      | `Command (_, `Delete_stream _) ->
        FLV.close_out dump
      | `Data [AMF.String l; AMF.String "onMetaData"; AMF.Map m] ->
        Printf.printf "matadata (%s): %s\n%!" l (AMF.to_string (AMF.Map m));
        FLV.write_metadata dump m
      | `Data amf ->
        Printf.printf "data: %s\n%!" (AMF.list_to_string amf)
      | `Command (_, `Result amf) ->
        Printf.printf "result\n%!"
      | _ -> assert false
    in
    Printf.printf "Accepting connection!\n%!";
    RTMP.handshake cnx;
    (* Let's go. *)
    while true do
      RTMP.read_chunk cnx;
      RTMP.handle_messages handler cnx
    done;
    Printf.printf "Done with connection\n%!";
    ignore (exit 0)
  done
