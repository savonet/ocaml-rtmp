(** Simple client. *)

open Rtmp

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
  let cnx = Rtmp.create_connection s in
  Rtmp.handshake cnx;
  Printf.printf "Handshake done.\n%!";
  let transaction_id =
    let n = ref 0 in
    fun () -> incr n; !n
  in
  let handle_message ~timestamp ~stream = function
    | `Command (_, `On_status amf) -> Printf.printf "On status: %s\n%!" (Amf.to_string amf)
    | `Command (_, `Result amf) -> Printf.printf "Got result: %s\n%!" (Amf.list_to_string (Array.to_list amf))
    | `Command (i, `On_bandwidth_done) -> Printf.printf "On bandwidth done\n%!"; Rtmp.command cnx "_checkbw" i [Amf.Null]
    | `Command (_, `Unhandled (name, amf)) -> Printf.printf "*** Unhandled command %s: %s\n%!" name (Amf.list_to_string (Array.to_list amf))
    | `Set_chunk_size n -> Rtmp.set_chunk_size cnx n
    | `Audio _ -> assert false
    | `Video _ -> assert false
    | `Data _ -> assert false
    | `Acknowledgement n -> Printf.printf "Acknowledgement %ld\n%!" n
    | _ -> Printf.printf "unhandled message...\n%!"
  in
  let poll ?(result=false) () =
    Rtmp.read_chunks ~handler:handle_message cnx
  in
  poll ();
  Printf.printf "Connecting.\n%!";
  Rtmp.command cnx "connect" (transaction_id ()) [Amf.Object ["app", Amf.String "live2"; "type", Amf.String "nonprivate"; "flashVer", Amf.String "FMLE/3.0"; "tcUrl", Amf.String url]];
  poll ~result:true ();
  Rtmp.command cnx "releaseStream" (transaction_id ()) [Amf.Null; Amf.String key];
  poll ();
  Rtmp.command cnx "FCPublish" (transaction_id ()) [Amf.Null; Amf.String key];
  poll ();
  Rtmp.command cnx "createStream" (transaction_id ()) [Amf.Null];
  poll ~result:true ();
  Rtmp.command cnx "publish" (transaction_id ()) [Amf.Null; Amf.String key; Amf.String "live"];
  poll ();
  let flv = Flv.open_in "test.flv" in
  let md = Flv.read_metadata flv in
  Printf.printf "set metadata to %s\n%!" (Amf.to_string md);
  Rtmp.data cnx [Amf.String "@setDataFrame"; Amf.String "onMetaData"; md];
  while true do
    poll ();
    Printf.printf "now: %ld\n%!" (Rtmp.now cnx);
    match Flv.read_tag flv with
    | timestamp, `Audio data ->
      Printf.printf "Send audio %ld: %d\n%!" timestamp (String.length data);
      Rtmp.audio cnx ~timestamp data
    | timestamp, `Video data ->
      Printf.printf "Send video %ld: %d\n%!" timestamp (String.length data);
      Rtmp.video cnx ~timestamp data
    | _, `Data _ -> assert false
  done

