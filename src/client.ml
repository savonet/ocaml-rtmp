(** Simple client. *)

let stream_key () =
  let ic = open_in "stream-key" in
  let key = input_line ic in
  close_in ic;
  key

let () =
  Random.self_init ();
  let key = stream_key () in
  let url = "rtmp://a.rtmp.youtube.com/live2" in
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
  let check_result () =
    assert (Unix.select [s] [] [] 60. <> ([], [], []));
    match RTMP.read_message cnx with
    | _, _, `Command(_, `Result _) -> Printf.printf "Connected!\n%!"
    | _ -> assert false
  in
  let poll () =
    RTMP.read_chunks cnx;
    let handler ~timestamp ~stream = function
      | `Command (_, `On_status amf) -> Printf.printf "On status: %s\n%!" (AMF.to_string amf)
      | _ -> Printf.printf "unhandled message...\n%!"; assert false
    in
    RTMP.handle_messages handler cnx
  in
  poll ();
  Printf.printf "Connecting.\n%!";
  RTMP.command cnx "connect" (transaction_id ()) [AMF.Object ["app", AMF.String "a.rtmp.youtube.com/live2"; "type", AMF.String "nonprivate"; "flashVer", AMF.String "FMLE/3.0"; "tcUrl", AMF.String url]];
  check_result ();
  RTMP.command cnx "releaseStream" (transaction_id ()) [AMF.Null; AMF.String key];
  poll ();
  RTMP.command cnx "FCPublish" (transaction_id ()) [AMF.Null; AMF.String key];
  poll ();
  RTMP.command cnx "createStream" (transaction_id ()) [AMF.Null];
  check_result ();
  (* TODO: metadata *)
  RTMP.command cnx "publish" (transaction_id ()) [AMF.Null; AMF.String key; AMF.String "live"];
  poll ()

