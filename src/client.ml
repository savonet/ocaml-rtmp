(** Simple client. *)

let stream_key () =
  let ic = open_in "stream-key" in
  let key = input_line ic in
  close_in ic;
  key

let () =
  Random.self_init ();
  let key = stream_key () in
  let url = "rtmp://a.rtmp.youtube.com:1935/live2" in
  (* let server = "localhost" in *)
  let server = "a.rtmp.youtube.com" in
  let addr = Unix.gethostbyname server in
  let s = Unix.socket addr.Unix.h_addrtype Unix.SOCK_STREAM 0 in
  Printf.printf "Connecting to %s... %!" server;
  Unix.connect s (Unix.ADDR_INET (addr.Unix.h_addr_list.(0), 1935));
  Printf.printf "done.\n%!";
  let cnx = RTMP.create_connection s in
  RTMP.handshake cnx;
  Printf.printf "Handshake done.\n%!";
  let transaction_id =
    let n = ref 0 in
    fun () -> incr n; !n
  in
  let handle_message ~timestamp ~stream = function
    | `Command (_, `On_status amf) -> Printf.printf "On status: %s\n%!" (AMF.to_string amf)
    | `Command (_, `Result amf) -> Printf.printf "Got result: %s\n%!" (AMF.list_to_string (Array.to_list amf))
    | `Command (i, `On_bandwidth_done) -> Printf.printf "On bandwidth done\n%!"; RTMP.command cnx "_checkbw" i [AMF.Null]
    | `Command (_, `Unhandled (name, amf)) -> Printf.printf "*** Unhandled command %s: %s\n%!" name (AMF.list_to_string (Array.to_list amf))
    | `Set_chunk_size n -> RTMP.set_chunk_size cnx n
    | `Audio _ -> assert false
    | `Video _ -> assert false
    | `Data _ -> assert false
    | `Acknowledgement n -> Printf.printf "Acknowledgement %ld\n%!" n
    | _ -> Printf.printf "unhandled message...\n%!"
  in
  let poll ?(result=false) () =
    RTMP.read_chunks ~handler:handle_message cnx
  in
  poll ();
  Printf.printf "Connecting.\n%!";
  RTMP.command cnx "connect" (transaction_id ()) [AMF.Object ["app", AMF.String "live2"; "type", AMF.String "nonprivate"; "flashVer", AMF.String "FMLE/3.0"; "tcUrl", AMF.String url]];
  poll ~result:true ();
  RTMP.command cnx "releaseStream" (transaction_id ()) [AMF.Null; AMF.String key];
  poll ();
  RTMP.command cnx "FCPublish" (transaction_id ()) [AMF.Null; AMF.String key];
  poll ();
  RTMP.command cnx "createStream" (transaction_id ()) [AMF.Null];
  poll ~result:true ();
  RTMP.command cnx "publish" (transaction_id ()) [AMF.Null; AMF.String key; AMF.String "live"];
  poll ();
  let flv = FLV.open_in "test.flv" in
  let md = FLV.read_metadata flv in
  Printf.printf "set metadata to %s\n%!" (AMF.to_string md);
  RTMP.data cnx [AMF.String "@setDataFrame"; AMF.String "onMetaData"; md];
  while true do
    poll ();
    match FLV.read_tag flv with
    | `Audio data ->
      Printf.printf "Send audio: %d\n%!" (String.length data);
      RTMP.audio cnx data
    | `Video data ->
      Printf.printf "Send video: %d\n%!" (String.length data);
      RTMP.video cnx data
    | `Data _ -> assert false
  done

