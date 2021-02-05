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

exception Connection_closed

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
  message_data : string list; (* Data in chunks, to be read from right to left (i.e. rev before concatenating). *)
  message_remaining : int; (* Bytes which remain to be fetched *)
}

module IMap = Map.Make (struct type t = int let compare = compare end)

(** Parameters for a given connection. *)
type connection =
  {
    mutable start_time : float;
    mutable last_time : float;
    socket : Unix.file_descr;
    mutable chunk_size : int;
    mutable last_timestamp : Int32.t IMap.t;
    mutable last_message_length : int IMap.t;
    mutable last_message_stream_id : Int32.t IMap.t;
    mutable last_message_type_id : int IMap.t;
    mutable partial_messages : message list; (* messages partially recieved *)
    messages : message Queue.t; (* messages recieved *)
  }

(** Create a connection. *)
let create_connection socket =
  {
    start_time = 0.;
    last_time = 0.;
    socket;
    chunk_size = 128;
    last_timestamp = IMap.empty;
    last_message_length = IMap.empty;
    last_message_stream_id = IMap.empty;
    last_message_type_id = IMap.empty;
    partial_messages = [];
    messages = Queue.create ();
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
  Printf.printf "\nCOMMAND: %s\n%!" name;
  let timestamp = now cnx in
  let data = AMF.encode_list ([AMF.String name; AMF.int transaction_id]@params) in
  chunkify cnx ~chunk_stream_id:3 ~timestamp ~message_type_id:0x14 ~message_stream_id data

let audio cnx ?(message_stream_id=Int32.zero) data =
  let timestamp = now cnx in
  chunkify cnx ~chunk_stream_id:4 ~timestamp ~message_type_id:0x08 ~message_stream_id data

let video cnx ?(message_stream_id=Int32.zero) data =
  let timestamp = now cnx in
  chunkify cnx ~chunk_stream_id:5 ~timestamp ~message_type_id:0x09 ~message_stream_id data

let data cnx ?(message_stream_id=Int32.zero) amf =
  let timestamp = now cnx in
  let data = AMF.encode_list amf in
  chunkify cnx ~chunk_stream_id:6 ~timestamp ~message_type_id:0x12 ~message_stream_id data

(** Perform handshake. *)
let handshake cnx =
  let s = cnx.socket in
  cnx.start_time <- Sys.time ();
  cnx.last_time <- cnx.start_time;
  (* S0 *)
  write_byte s 3;
  (* S1 *)
  let time2 = (* now cnx *) Int32.zero in
  let rand2 = Bytes.create 1528 in
  write_int32 s time2;
  write_int32 s Int32.zero;
  write s rand2;
  (* C0 *)
  let c0 = read_byte s in
  assert (c0 = 3);
  Printf.printf "C0\n%!";
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
  let add_message msg =
    match msg.message_type_id with
    | 0x01 ->
      (* We need to handle this one ASAP because it has influence on how we read next messages... *)
      let data = String.concat "" (List.rev msg.message_data) in
      let n = int32_of_bits data in
      Printf.printf "Got chunk size: %ld\n%!" n;
      cnx.chunk_size <- Int32.to_int n
    | _ ->
      if msg.message_remaining = 0 then Queue.add msg cnx.messages
      else cnx.partial_messages <- msg :: cnx.partial_messages
  in
  let pop_message cid =
    let f, m = List.partition (fun msg -> msg.message_chunk_stream_id = cid) cnx.partial_messages in
    if (List.length f = 0) then raise Not_found;
    assert (List.length f = 1);
    cnx.partial_messages <- m;
    List.hd f
  in
  let have_message cid = List.exists (fun msg -> msg.message_chunk_stream_id = cid) cnx.partial_messages in
  let s = cnx.socket in
  (* Basic header *)
  let basic = try read_byte s with _ -> raise Connection_closed in
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
      cnx.last_timestamp <- IMap.add chunk_stream_id timestamp cnx.last_timestamp;
      cnx.last_message_length <- IMap.add chunk_stream_id message_length cnx.last_message_length;
      cnx.last_message_stream_id <- IMap.add chunk_stream_id message_stream_id cnx.last_message_stream_id;
      cnx.last_message_type_id <- IMap.add chunk_stream_id message_type_id cnx.last_message_type_id;
      Printf.printf "Timestamp: %ld\n%!" timestamp;
      Printf.printf "Message length: %d\n%!" message_length;
      Printf.printf "Message type: %d (0x%x)\n%!" message_type_id message_type_id;
      Printf.printf "Message stream: %ld\n%!" message_stream_id;
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
      Printf.printf "Timestamp delta: %ld\n%!" timestamp_delta;
      Printf.printf "Message length: %d\n%!" message_length;
      Printf.printf "Message type: %d (0x%x)\n%!" message_type_id message_type_id;
      let timestamp = IMap.find chunk_stream_id cnx.last_timestamp in
      let timestamp = Int32.add timestamp timestamp_delta in
      cnx.last_timestamp <- IMap.add chunk_stream_id timestamp cnx.last_timestamp;
      cnx.last_message_length <- IMap.add chunk_stream_id message_length cnx.last_message_length;
      cnx.last_message_type_id <- IMap.add chunk_stream_id message_type_id cnx.last_message_type_id;
      let data_length = min cnx.chunk_size message_length in
      let data = read_string s data_length in
      let msg =
        {
          message_chunk_stream_id = chunk_stream_id;
          message_timestamp = timestamp;
          message_type_id;
          message_stream_id = IMap.find chunk_stream_id cnx.last_message_stream_id;
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
      let timestamp = IMap.find chunk_stream_id cnx.last_timestamp in
      let timestamp = Int32.add timestamp timestamp_delta in
      cnx.last_timestamp <- IMap.add chunk_stream_id timestamp cnx.last_timestamp;
      Printf.printf "Timestamp delta: %ld\n%!" timestamp_delta;
      let message_type_id = IMap.find chunk_stream_id cnx.last_message_type_id in
      let message_length = IMap.find chunk_stream_id cnx.last_message_length in
      Printf.printf "Length (copied): %d\n%!" message_length;
      let data_length = min cnx.chunk_size message_length in
      let data = read_string s data_length in
      let msg =
        {
          message_chunk_stream_id = chunk_stream_id;
          message_timestamp = timestamp;
          message_type_id;
          message_stream_id = IMap.find chunk_stream_id cnx.last_message_stream_id;
          message_data = [data];
          message_remaining = message_length - data_length;
        }
      in
      add_message msg
    | 3 ->
      if have_message chunk_stream_id then
        let msg = pop_message chunk_stream_id in
        let remaining = msg.message_remaining in
        let data_length = min cnx.chunk_size remaining in
        Printf.printf "read %d\n%!" data_length;
        let data = read_string s data_length in
        add_message { msg with message_data = data :: msg.message_data; message_remaining = msg.message_remaining - data_length }
      else
        let timestamp = IMap.find chunk_stream_id cnx.last_timestamp in (* TODO: do we increment automatically? *)
        let message_type_id = IMap.find chunk_stream_id cnx.last_message_type_id in
        let message_length = IMap.find chunk_stream_id cnx.last_message_length in
        let data_length = min cnx.chunk_size message_length in
        let data = read_string s data_length in
        let msg =
          {
            message_chunk_stream_id = chunk_stream_id;
            message_timestamp = timestamp;
            message_type_id;
            message_stream_id = IMap.find chunk_stream_id cnx.last_message_stream_id;
            message_data = [data];
            message_remaining = message_length - data_length;
          }
        in
        Printf.printf "No message found, creating a new one\n%!";
        add_message msg
    | _ -> failwith ("TODO: Handle chunk type " ^ string_of_int chunk_type)
  )

(** Read as many chunks as available. *)
let read_chunks cnx =
  let s = cnx.socket in
  while Unix.select [s] [] [] 0.1 <> ([], [], []) do
    read_chunk cnx
  done

let has_message cnx =
  not (Queue.is_empty cnx.messages)

let pop_message cnx =
  let msg = Queue.pop cnx.messages in
  let data = String.concat "" (List.rev msg.message_data) in
  msg.message_timestamp, msg.message_stream_id,
  match msg.message_type_id with
    (*
    | 0x01 ->
      let n = int32_of_bits data in
      Printf.printf "Got chunk size: %ld\n%!" n;
      cnx.chunk_size <- Int32.to_int n
    *)
  | 0x05 ->
    let n = int32_of_bits data in
    Printf.printf "Got window ack: %ld\n%!" n;
    `Window_acknowledgement_size n
  | 0x06 ->
    let n = int32_of_bits (String.sub data 0 4) in
    let t = int_of_char data.[4] in
    Printf.printf "Peer bandwidth: %ld / %d\n%!" n t;
    let t = match t with 0 -> `Soft | 1 -> `Hard | 2 -> `Dynamic | _ -> assert false in
    `Peer_banddwidth (n, t)
  | 0x08 ->
    Printf.printf "Audio message (%d bytes)\n%!" (String.length data);
    `Audio data
  | 0x09 ->
    Printf.printf "Video message (%d bytes)\n%!" (String.length data);
    `Video data
  | 0x12 ->
    (* Printf.printf "Data: %s\n%!" data; *)
    let amf = AMF.decode data in
    Printf.printf "Data: %s\n%!" (AMF.list_to_string amf);
    `Data amf
  | 0x14 ->
    (* Printf.printf "AMF0: %s\n%!" data; *)
    (* Printf.printf "%s\n%!" (hex_of_string data); *)
    let amf = AMF.decode data in
    Printf.printf "%s\n%!" (AMF.list_to_string amf);
    let amf = Array.of_list amf in
    let tid = AMF.get_int amf.(1) in
    (
      match AMF.get_string amf.(0) with
      | "connect" ->
        `Command (tid, `Connect)
      | "createStream" ->
        `Command (tid, `Create_stream)
      | "deleteStream" ->
        Printf.printf "Deleting stream...\n%!";
        let n = AMF.get_int amf.(3) in
        `Command (tid, `Delete_stream n)
      | "publish" ->
        Printf.printf "Publish\n%!";
        let name = AMF.get_string amf.(3) in
        let kind = AMF.get_string amf.(4) in
        `Command (tid, `Publish (name, kind))
      | "onStatus" ->
        Printf.printf "On status\n%!";
        `Command (tid, `On_status amf.(3))
      | "_result" ->
        Printf.printf "Result\n%!";
        let amf = Array.sub amf 2 (Array.length amf - 2) in
        `Command (tid, `Result amf)
      | name ->
        let amf = Array.sub amf 2 (Array.length amf - 2) in
        Printf.printf "Unhandled AMF: %s (%s)\n%!" (AMF.to_string amf.(0)) (AMF.list_to_string (Array.to_list amf));
        `Command (tid, `Unhandled (name, amf))
    )
  | t -> Printf.printf "\nUnhandled message type 0x%02x\n%!" t; assert false

let handle_messages cnx handler =
  while has_message cnx do
    let timestamp, stream, msg = pop_message cnx in
    handler ~timestamp ~stream msg
  done
