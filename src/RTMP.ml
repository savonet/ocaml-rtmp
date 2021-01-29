(* We are following
   - https://www.adobe.com/devnet/rtmp.html
   - https://www.adobe.com/content/dam/acom/en/devnet/pdf/amf0-file-format-specification.pdf
   - https://www.adobe.com/content/dam/acom/en/devnet/pdf/amf-file-format-spec.pdf
   - https://github.com/illuspas/Node-Media-Server/blob/master/node_rtmp_session.js
*)

(* A few general remarks:
   - all integers a big-endian
   - timestamps are in miliseconds
*)

open IO

(* Chunk streams id:
   - 2: protocol
   - 3: invoke
   - 4: audio
   - 5: video
   - 6: data
*)

(** Low-level functions. *)

let basic_header f ~chunk_type ~chunk_stream_id =
  assert (0 <= chunk_type && chunk_type < 4);
  assert (2 <= chunk_stream_id && chunk_stream_id <= 65599);
  if chunk_stream_id <= 63 then
    write_byte f ((chunk_type lsl 6) + chunk_stream_id)
  else if chunk_stream_id <= 319 then (
    write_byte f (chunk_type lsl 6);
    write_byte f (chunk_stream_id - 64) )
  else (
    write_byte f ((chunk_type lsl 6) + 0x111111);
    write_short f chunk_stream_id )

let chunk_header0 f ~chunk_stream_id ~timestamp ~message_length ~message_type_id ~message_stream_id =
  basic_header f ~chunk_type:0 ~chunk_stream_id;
  let extended = timestamp >= Int32.of_int 0xffffff in
  let timestamp' = if extended then 0xffffff else Int32.to_int timestamp in
  write_int24 f timestamp';
  write_int24 f message_length;
  write_byte f message_type_id;
  write_int32_le f message_stream_id;
  if extended then write_int32 f timestamp

let chunk_header1 f ~chunk_stream_id ~timestamp_delta ~message_length ~message_type_id =
  basic_header f ~chunk_type:1 ~chunk_stream_id;
  let extended = timestamp_delta >= Int32.of_int 0xffffff in
  let timestamp_delta' =
    if extended then 0xffffff else Int32.to_int timestamp_delta
  in
  write_int24 f timestamp_delta';
  write_int24 f message_length;
  write_byte f message_type_id;
  if extended then write_int32 f timestamp_delta

let chunk_header2 f ~chunk_stream_id ~timestamp_delta =
  basic_header f ~chunk_type:2 ~chunk_stream_id;
  let extended = timestamp_delta >= Int32.of_int 0xffffff in
  let timestamp_delta' =
    if extended then 0xffffff else Int32.to_int timestamp_delta
  in
  write_int24 f timestamp_delta';
  if extended then write_int32 f timestamp_delta

let chunk_header3 f ~chunk_stream_id =
  basic_header f ~chunk_type:3 ~chunk_stream_id

(** High-level functions. *)

(** A (partially received) message. *)
type message = {
  message_chunk_stream_id : int;
  message_timestamp : Int32.t;
  message_type_id : int;
  message_stream_id : Int32.t;
  mutable message_data : string list; (* Data in chunks, to be read from right to left (i.e. rev before concatenating). *)
  mutable message_remaining : int; (* Bytes which remain to be fetched *)
}

(** Parameters for a given connection. *)
type connection =
  {
    mutable start_time : float;
    mutable last_time : float;
    socket : Unix.file_descr;
    mutable chunk_size : int;
    mutable last_timestamp : Int32.t;
    mutable last_message_length : int;
    mutable last_message_stream_id : Int32.t;
    mutable last_message_type_id : int;
    mutable messages : message list;
  }

(** Create a connection. *)
let create_connection socket =
  {
    start_time = 0.;
    last_time = 0.;
    socket;
    chunk_size = 128;
    last_timestamp = Int32.zero;
    last_message_length = 0;
    last_message_stream_id = Int32.zero;
    last_message_type_id = 0;
    messages = [];
  }

(** Current timestamp. *)
let now cnx = Int32.of_float ((Sys.time () -. cnx.start_time) *. 1000.)

(** Current timestamp delta. *)
let delta cnx = Int32.of_float ((Sys.time () -. cnx.last_time) *. 1000.)

(** Write a long message with chunks. *)
let chunkify cnx ~chunk_stream_id ~timestamp ~message_type_id ~message_stream_id data =
  let data = Bytes.unsafe_of_string data in
  let message_length = Bytes.length data in
  chunk_header0 cnx.socket ~chunk_stream_id ~timestamp ~message_length ~message_type_id ~message_stream_id;
  let rem = message_length in
  let len = min rem cnx.chunk_size in
  assert (Unix.write cnx.socket data 0 len = len);
  let rem = ref (rem - len) in
  while !rem > 0 do
    let len = min !rem cnx.chunk_size in
    chunk_header3 cnx.socket ~chunk_stream_id;
    assert (Unix.write cnx.socket data (message_length - !rem) len = len);
    rem := !rem - len
  done

(** Send a control message. *)
let control_message cnx message_type_id payload =
  let timestamp = now cnx in
  assert (String.length payload <= cnx.chunk_size); (* TODO *)
  chunk_header0 cnx.socket ~chunk_stream_id:2 ~timestamp ~message_stream_id:Int32.zero ~message_type_id ~message_length:(String.length payload);
  write cnx.socket (Bytes.unsafe_of_string payload)

let set_chunk_size f n =
  assert (1 <= n && n <= 0x7fffffff);
  Printf.printf "Send set chunk size: %d\n%!" n;
  let s = Bytes.create 4 in
  Bytes.set s 0 (char_of_int ((n lsr 24) land 0xff));
  Bytes.set s 1 (char_of_int ((n lsr 16) land 0xff));
  Bytes.set s 2 (char_of_int ((n lsr 8) land 0xff));
  Bytes.set s 3 (char_of_int (n land 0xff));
  control_message f 1 (Bytes.unsafe_to_string s)

let abort_message f n = control_message f 2 (bits_of_int32 n)

let acknowledgement f n = control_message f 3 (bits_of_int32 n)

(** Send a user control message. *)
let user_control cnx t data =
  control_message cnx 4 (bits_of_int16 t);
  assert (String.length data + 2 <= cnx.chunk_size); (* TODO *)
  write cnx.socket (Bytes.unsafe_of_string data)

let stream_begin f stream_id = user_control f 0 (bits_of_int32 stream_id)
let ping_request f timestamp = user_control f 6 (bits_of_int32 timestamp)
let ping_response f timestamp = user_control f 7 (bits_of_int32 timestamp)

let window_acknowledgement_size f n =
  Printf.printf "Send window acknowledgement size: %d\n%!" n;
  control_message f 5 (bits_of_int32 (Int32.of_int n))

let set_peer_bandwidth f n t =
  Printf.printf "Send set peer bandwidth: %d\n%!" n;
  let n = bits_of_int32 (Int32.of_int n) in
  let t = match t with `Hard -> 0 | `Soft -> 1 | `Dynamic -> 2 in
  let t = String.make 1 (char_of_int t) in
  control_message f 6 (n ^ t)

let command cnx ?(message_stream_id=Int32.zero) name transaction_id params =
  let timestamp = now cnx in
  let data = AMF.encode_list ([AMF.String name; AMF.Number transaction_id]@params) in
  assert (String.length data <= cnx.chunk_size);
  chunk_header0 cnx.socket ~chunk_stream_id:3 ~timestamp ~message_type_id:0x14 ~message_stream_id ~message_length:(String.length data);
  write cnx.socket (Bytes.unsafe_of_string data)

(** Perform handshake. *)
let handshake cnx =
  let s = cnx.socket in
  cnx.start_time <- Sys.time ();
  cnx.last_time <- cnx.start_time;
  (* C0 *)
  let c0 = read_byte s in
  assert (c0 = 3);
  Printf.printf "C0\n%!";
  (* S0 *)
  write_byte s 3;
  (* S1 *)
  let time2 = (* now cnx *) Int32.zero in
  let rand2 = Bytes.create 1528 in
  write_int32 s time2;
  write_int32 s Int32.zero;
  write s rand2;
  (* C1 *)
  let time = read_int32 s in
  let time2' = read_int32 s in (* should be zero *)
  let rand = read s 1528 in
  (* assert (time2' = Int32.zero); *)
  Printf.printf "C1 (%ld, %ld)\n%!" time time2';
  (* S2 *)
  write_int32 s time2;
  write_int32 s time;
  write s rand;
  (* C2 *)
  let time' = read_int32 s in
  if time' <> time then
    Printf.printf "C2 time: %ld instead of %ld\n%!" time' time;
  assert (read_int32 s = time2);
  assert (read s 1528 = rand2);
  Printf.printf "C2 (%ld)\n%!" time'

(** Read a chunk. *)
let read_chunk cnx =
  Printf.printf "\n%!";
  let add_message msg = cnx.messages <- msg :: cnx.messages in
  let find_message cid = List.find (fun msg -> msg.message_chunk_stream_id = cid) cnx.messages in
  let s = cnx.socket in
  (* Basic header *)
  let basic = read_byte s in
  let chunk_type = basic lsr 6 in
  Printf.printf "Chunk type: %d\n%!" chunk_type;
  let chunk_stream_id =
    let n = basic land 0x3f in
    if n = 0 then read_byte s + 64
    else if n = 0x3f then read_short s + 64
    else n
  in
  Printf.printf "Chunk stream id: %d\n%!" chunk_stream_id;
  (* Message header *)
  ( match chunk_type with
    | 0 ->
      let timestamp = read_int24 s in
      let message_length = read_int24 s in
      let message_type_id = read_byte s in
      let message_stream_id = read_int32_le s in
      let timestamp =
        if timestamp = 0xffffff then read_int32 s
        else Int32.of_int timestamp
      in
      cnx.last_timestamp <- timestamp;
      cnx.last_message_length <- message_length;
      cnx.last_message_stream_id <- message_stream_id;
      cnx.last_message_type_id <- message_type_id;
      Printf.printf "Timestamp: %d\n%!" (Int32.to_int timestamp);
      Printf.printf "Message length: %d\n%!" message_length;
      Printf.printf "Message type: %d (0x%x)\n%!" message_type_id message_type_id;
      Printf.printf "Message stream: %d\n%!" (Int32.to_int message_stream_id);
      let data_length = min cnx.chunk_size message_length in
      let data = read_string s data_length in
      let msg =
        {
          message_chunk_stream_id = chunk_stream_id;
          message_timestamp = timestamp;
          message_type_id;
          message_stream_id;
          message_data = [data];
          message_remaining = message_length - data_length;
        }
      in
      add_message msg
    | 1 ->
      let timestamp_delta = read_int24 s in
      let message_length = read_int24 s in
      let message_type_id = read_byte s in
      let timestamp_delta =
        if timestamp_delta = 0xffffff then read_int32 s
        else Int32.of_int timestamp_delta
      in
      let timestamp = Int32.add cnx.last_timestamp timestamp_delta in
      cnx.last_timestamp <- timestamp;
      cnx.last_message_length <- message_length;
      cnx.last_message_type_id <- message_type_id;
      Printf.printf "Timestamp delta: %d\n%!" (Int32.to_int timestamp_delta);
      Printf.printf "Message length: %d\n%!" message_length;
      Printf.printf "Message type: %d (0x%x)\n%!" message_type_id message_type_id;
      let data_length = min cnx.chunk_size message_length in
      let data = read_string s data_length in
      let msg =
        {
          message_chunk_stream_id = chunk_stream_id;
          message_timestamp = timestamp;
          message_type_id;
          message_stream_id = cnx.last_message_stream_id;
          message_data = [data];
          message_remaining = message_length - data_length;
        }
      in
      add_message msg
    | 2 ->
      let timestamp_delta = read_int24 s in
      let timestamp_delta =
        if timestamp_delta = 0xffffff then read_int32 s
        else Int32.of_int timestamp_delta
      in
      let timestamp = Int32.add cnx.last_timestamp timestamp_delta in
      cnx.last_timestamp <- timestamp;
      Printf.printf "Timestamp delta: %d\n%!" (Int32.to_int timestamp_delta);
      let message_type_id = cnx.last_message_type_id in
      let message_length = cnx.last_message_length in
      let data_length = min cnx.chunk_size message_length in
      let data = read_string s data_length in
      let msg =
        {
          message_chunk_stream_id = chunk_stream_id;
          message_timestamp = timestamp;
          message_type_id;
          message_stream_id = cnx.last_message_stream_id;
          message_data = [data];
          message_remaining = message_length - data_length;
        }
      in
      add_message msg
    | 3 ->
      let msg = find_message chunk_stream_id in
      let remaining = msg.message_remaining in
      let data_length = min cnx.chunk_size remaining in
      Printf.printf "read %d\n%!" data_length;
      let data = read_string s data_length in
      msg.message_data <- data :: msg.message_data;
      msg.message_remaining <- msg.message_remaining - data_length
    | _ -> failwith ("TODO: Handle chunk type " ^ string_of_int chunk_type)
  )

let client () =
  Random.self_init ();
  let url = "rtmp://a.rtmp.youtube.com/live2" in
  let server = "a.rtmp.youtube.com" in
  let addr = Unix.gethostbyname server in
  let s = Unix.socket addr.Unix.h_addrtype Unix.SOCK_STREAM 0 in
  Unix.connect s (Unix.ADDR_INET (addr.Unix.h_addr_list.(0), 1935));
  let cnx = create_connection s in
  handshake cnx;
  let transaction_id =
    let n = ref 0 in
    fun () -> incr n; float_of_int !n
  in
  command cnx "connect" (transaction_id ()) [AMF.Object ["app", AMF.String "youtube"; "tcUrl", AMF.String url]];
  command cnx "releaseStream" (transaction_id ()) [AMF.Null; AMF.String "live2"];
  command cnx "FCPublish" (transaction_id ()) [AMF.Null; AMF.String "live2"];
  command cnx "createStream" (transaction_id ()) [AMF.Null];
  command cnx "publish" (transaction_id ()) [AMF.Null; AMF.String "live2"; AMF.String "live"]

(** Local server. *)
let server () =
  let dump = open_out "dump.flv" in
  output_string dump "FLV\x01";
  output_string dump "\x01";
  output_string dump (bits_of_int32 (Int32.of_int 9));
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 1935));
  Unix.listen socket 5;
  while true do
    Printf.printf "Waiting for client\n%!";
    let s, caller = Unix.accept socket in
    let cnx = create_connection s in
    Printf.printf "Accepting connection!\n%!";
    handshake cnx;
    (* Let's go. *)
    let handle_message msg =
      let data = String.concat "" (List.rev msg.message_data) in
      match msg.message_type_id with
      | 0x01 ->
        let n = int32_of_bits data in
        Printf.printf "Got chunk size: %ld\n%!" n;
        cnx.chunk_size <- Int32.to_int n
      | 0x09 ->
        Printf.printf "Video message (%d bytes)\n%!" (String.length data);
        output_string dump (bits_of_int32 Int32.zero); (* size of previous tag *)
        (* output_string dump (bits_of_int32 Int32.zero); (\* size of previous tag *\) *)
        output_string dump "\x09"; (* video *)
        output_string dump (bits_of_int24 (String.length data));
        output_string dump (bits_of_int24 (Int32.to_int (now cnx))); (* timestamp *)
        (* output_string dump "\0x00"; (\* higher bit of timestamp *\) *)
        (* output_string dump (bits_of_int24 0); (\* stream id, always 0 *\) *)
        output_string dump "\x00\x00\x00\x00";
        output_string dump data
      | 0x12 ->
        Printf.printf "Data: %s\n%!" data;
        let amf = AMF.decode data in
        Printf.printf "%s\n%!" (AMF.list_to_string amf)
      | 0x14 ->
        Printf.printf "AMF0: %s\n%!" data;
        Printf.printf "%s\n%!" (hex_of_string data);
        let amf = AMF.decode data in
        Printf.printf "%s\n%!" (AMF.list_to_string amf);
        let amf = Array.of_list amf in
        (
          match AMF.get_string amf.(0) with
          | "connect" ->
            Printf.printf "Connecting...\n%!";
            window_acknowledgement_size cnx 500000;
            set_peer_bandwidth cnx 500000 `Dynamic;
            set_chunk_size cnx cnx.chunk_size;
            let tid = AMF.get_number amf.(1) in
            command cnx ~message_stream_id:Int32.zero "_result" tid [AMF.Object ["fmsVer", AMF.String "FMS/3,0,1,123"; "capabilities", AMF.Number 31.]]
          | "createStream" ->
            Printf.printf "Creating stream...\n%!";
            let tid = AMF.get_number amf.(1) in
            let stream_id = 0 in
            command cnx ~message_stream_id:Int32.zero "_result" tid [AMF.Null; AMF.Number (float_of_int stream_id)]
          | "deleteStream" ->
            Printf.printf "Deleting stream...\n%!";
            close_out dump
          | "publish" ->
            Printf.printf "Publishing...\n%!";
            let tid = AMF.get_number amf.(1) in
            let name = AMF.get_string amf.(3) in
            let kind = AMF.get_string amf.(4) in
            let stream_id = 0 in
            command cnx ~message_stream_id:Int32.zero "onStatus" tid [AMF.Null; AMF.Object ["level", AMF.String "status"; "code", AMF.String "NetStream.Publish.Start"; "descritpion", AMF.String ("Publishing stream " ^ name)]]
            (* stream_begin cnx.socket (Int32.of_int stream_id) *)
          | _ ->
            Printf.printf "Unhandled AMF: %s\n%!" (AMF.to_string amf.(0))
        )
      | t -> Printf.printf "\nUnhandled message type 0x%02x\n%!" t; assert false
    in
    let handle_messages () =
      cnx.messages <-
        List.filter
          (fun msg ->
             if msg.message_remaining = 0 then (
               handle_message msg;
               false )
             else true)
          cnx.messages
    in
    while true do
      read_chunk cnx;
      handle_messages ()
    done;
    Printf.printf "Done with connection\n%!";
    ignore (exit 0)
  done

let () =
  server ()
