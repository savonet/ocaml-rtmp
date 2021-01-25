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

let now () = Int32.of_float (Sys.time () *. 1000.)

(** High-level functions. *)

(** Parameters for a given connection. *)
type connection =
  {
    socket : Unix.file_descr;
    mutable chunk_size : int;
    mutable last_timestamp : Int32.t;
    mutable last_message_length : int;
    mutable last_message_stream_id : Int32.t;
  }

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

let control_message f message_type_id payload =
  chunk_header0 f ~chunk_stream_id:2 ~timestamp:Int32.zero ~message_stream_id:Int32.zero ~message_type_id ~message_length:(String.length payload);
  write f (Bytes.unsafe_of_string payload)
  (* chunkify f ~chunk_stream_id:2 ~timestamp:Int32.zero ~message_stream_id:Int32.zero ~message_type_id payload *)

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

let user_control f t data =
  control_message f 4 (bits_of_int16 t);
  write f (Bytes.unsafe_of_string data)

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

let command f ~message_stream_id name transaction_id params =
  let data = AMF.encode_list ([AMF.String name; AMF.Number transaction_id]@params) in
  chunk_header0 f ~chunk_stream_id:3 ~timestamp:Int32.zero ~message_type_id:0x14 ~message_stream_id ~message_length:(String.length data);
  write f (Bytes.unsafe_of_string data)

(* rtmp://a.rtmp.youtube.com/live2 *)
let test () =
  Random.self_init ();
  let server = "a.rtmp.youtube.com" in
  let addr = Unix.gethostbyname server in
  let s = Unix.socket addr.Unix.h_addrtype Unix.SOCK_STREAM 0 in
  Unix.connect s (Unix.ADDR_INET (addr.Unix.h_addr_list.(0), 1935));
  let time = Int32.zero in
  (* time origin *)
  (* C0 *)
  write_byte s 3;
  (* C1 *)
  write_int32 s time;
  write_int32 s Int32.zero;
  for i = 0 to 1527 do
    write_byte s (Random.int 256)
  done;
  (* S0 *)
  assert (read_byte s = 3);
  (* S1 *)
  assert (read_int32 s = time);
  let time2 = read_int32 s in
  let rand = read s 1528 in
  (* C2 *)
  write_int32 s time;
  write_int32 s time2;
  write s rand;
  (* S2 *)
  ignore (read s (4 + 4 + 1158))

type message = {
  message_chunk_stream_id : int;
  message_timestamp : Int32.t;
  message_type_id : int;
  message_stream_id : Int32.t;
  mutable message_data : string list;
  (* Data in chunks, to be read from right to left (i.e. rev before concatenating). *)
  mutable message_remaining : int; (* Bytes which remain to be fetched *)
}

(** Local server. *)
let () =
  let dump = open_out "dump" in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 1935));
  Unix.listen socket 5;
  let cnx =
    {
      socket;
      chunk_size = 128;
      last_timestamp = Int32.zero;
      last_message_stream_id = Int32.zero;
      last_message_length = 0;
    }
  in
  while true do
    Printf.printf "Waiting for client\n%!";
    let s, caller = Unix.accept cnx.socket in
    Printf.printf "Accepting connection!\n%!";
    (* C0 *)
    let c0 = read_byte s in
    assert (c0 = 3);
    Printf.printf "C0\n%!";
    (* S0 *)
    write_byte s 3;
    (* S1 *)
    let time2 = (* now () *) Int32.zero in
    let rand2 = Bytes.create 1528 in
    write_int32 s time2;
    write_int32 s Int32.zero;
    write s rand2;
    (* C1 *)
    let time = read_int32 s in
    let (* zero *) _ = read_int32 s in
    let rand = read s 1528 in
    (* assert (zero = Int32.zero); *)
    Printf.printf "C1\n%!";
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
    Printf.printf "C2\n%!";
    (* Let's go. *)
    let messages = ref [] in
    let add_message msg = messages := msg :: !messages in
    let find_message cid =
      List.find (fun msg -> msg.message_chunk_stream_id = cid) !messages
    in
    let handle_message msg =
      let data = String.concat "" (List.rev msg.message_data) in
      match msg.message_type_id with
      | 0x01 ->
        let n = int32_of_bits data in
        Printf.printf "Got chunk size: %ld\n%!" n;
        cnx.chunk_size <- Int32.to_int n
      | 0x09 ->
        Printf.printf "Video message (%d bytes)\n%!" (String.length data);
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
            window_acknowledgement_size s 500000;
            set_peer_bandwidth s 500000 `Dynamic;
            set_chunk_size s cnx.chunk_size;
            let tid = AMF.get_number amf.(1) in
            command s ~message_stream_id:Int32.zero "_result" tid [AMF.Object ["fmsVer", AMF.String "FMS/3,0,1,123"; "capabilities", AMF.Number 31.]]
          | "createStream" ->
            Printf.printf "Creating stream...\n%!";
            let tid = AMF.get_number amf.(1) in
            let stream_id = 0 in
            command s ~message_stream_id:Int32.zero "_result" tid [AMF.Null; AMF.Number (float_of_int stream_id)]
          | "deleteStream" ->
            Printf.printf "Deleting stream...\n%!";
            close_out dump
          | "publish" ->
            Printf.printf "Publishing...\n%!";
            let tid = AMF.get_number amf.(1) in
            let name = AMF.get_string amf.(3) in
            let kind = AMF.get_string amf.(4) in
            let stream_id = 0 in
            command s ~message_stream_id:Int32.zero "onStatus" tid [AMF.Null; AMF.Object ["level", AMF.String "status"; "code", AMF.String "NetStream.Publish.Start"; "descritpion", AMF.String ("Publishing stream " ^ name)]]
            (* stream_begin cnx.socket (Int32.of_int stream_id) *)
          | _ ->
            Printf.printf "Unhandled AMF: %s\n%!" (AMF.to_string amf.(0))
        )
      | _ -> assert false
    in
    let handle_messages () =
      messages :=
        List.filter
          (fun msg ->
             if msg.message_remaining = 0 then (
               handle_message msg;
               false )
             else true)
          !messages
    in
    while true do
      Printf.printf "\n%!";
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
        (*
        | 2 ->
          let timestamp_delta = read_int24 s in
          let timestamp_delta =
            if timestamp_delta = 0xffffff then read_int32 s
            else Int32.of_int timestamp_delta
          in
          let timestamp = Int32.add cnx.last_timestamp timestamp_delta in
          let message_type_id = cnx.la
          let message_length = cnx.last_message_length in
          cnx.last_timestamp <- timestamp;
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
          *)
        | 3 ->
          let msg = find_message chunk_stream_id in
          let remaining = msg.message_remaining in
          let data_length = min cnx.chunk_size remaining in
          Printf.printf "read %d\n%!" data_length;
          let data = read_string s data_length in
          msg.message_data <- data :: msg.message_data;
          msg.message_remaining <- msg.message_remaining - data_length
        | _ -> failwith ("TODO: Handle chunk type " ^ string_of_int chunk_type)
      );
      handle_messages ()
    done;
    Printf.printf "Done with connection\n%!";
    ignore (exit 0)
  done
