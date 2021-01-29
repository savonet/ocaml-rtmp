(** Simple client. *)

let () =
  Random.self_init ();
  let url = "rtmp://a.rtmp.youtube.com/live2" in
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
  let poll () =
    RTMP.read_chunks cnx;
    let handler ~timestamp ~stream = function
      | _ -> Printf.printf "unhandled message...\n%!"; assert false
    in
    RTMP.handle_messages handler cnx
  in
  RTMP.command cnx "connect" (transaction_id ()) [AMF.Object ["app", AMF.String "youtube"; "tcUrl", AMF.String url]];
  poll ();
  RTMP.command cnx "releaseStream" (transaction_id ()) [AMF.Null; AMF.String "live2"];
  poll ();
  RTMP.command cnx "FCPublish" (transaction_id ()) [AMF.Null; AMF.String "live2"];
  poll ();
  RTMP.command cnx "createStream" (transaction_id ()) [AMF.Null];
  poll ();
  RTMP.command cnx "publish" (transaction_id ()) [AMF.Null; AMF.String "live2"; AMF.String "live"];
  poll ()

