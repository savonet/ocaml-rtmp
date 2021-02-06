(** Operations on flv files. *)

open Io

let byte n = String.make 1 (char_of_int n)

type out_t = out_channel * int ref (* size of previous tag *)

let oc (f : out_t) = fst f

(** Previous tag size. *)
let pts (f : out_t) = snd f

let open_out ?(audio = true) ?(video = true) fname : out_t =
  let oc = open_out fname in
  output_string oc "FLV\x01";
  output_string oc (byte ((if audio then 4 else 0) + if video then 1 else 0));
  output_string oc (bits_of_int32 (Int32.of_int 9));
  (oc, ref 0)

let close_out f = close_out (oc f)

let write_tag f tag timestamp data =
  let oc = oc f in
  let s = pts f in
  output_string oc (bits_of_int32 (Int32.of_int !s));
  (* size of previous tag *)
  s := 11 + String.length data;
  output_string oc (byte tag);
  output_string oc (bits_of_int24 (String.length data));
  output_string oc (bits_of_int24 (Int32.to_int timestamp));
  (* timestamp *)
  (* output_string oc "\0x00"; (\* higher bit of timestamp *\) *)
  (* output_string oc (bits_of_int24 0); (\* stream id, always 0 *\) *)
  output_string oc "\x00\x00\x00\x00";
  output_string oc data

let write_data f l =
  let data = Amf.encode_list l in
  write_tag f 18 Int32.zero data

let write_metadata f l =
  let l = [Amf.String "onMetaData"; Amf.Map l] in
  write_data f l

let write_audio f timestamp data = write_tag f 8 timestamp data
let write_video f timestamp data = write_tag f 9 timestamp data

type in_t = in_channel

let input_u24 ic =
  let ans = ref 0 in
  for _ = 0 to 2 do
    ans := (!ans lsl 8) + input_byte ic
  done;
  !ans

let input_u32 ic =
  let ans = ref Int32.zero in
  for _ = 0 to 3 do
    ans := Int32.add (Int32.shift_left !ans 8) (Int32.of_int (input_byte ic))
  done;
  !ans

let open_in fname : in_t =
  let ic = open_in fname in
  assert (really_input_string ic 3 = "FLV");
  assert (input_byte ic = 0x01);
  ignore (input_byte ic);
  assert (input_u32 ic = Int32.of_int 9);
  (* Size of previous tag *)
  assert (input_u32 ic = Int32.zero);
  ic

let read_tag (ic : in_t) =
  let n = input_byte ic in
  (* Reserved *)
  assert (n land 0b11000000 = 0);
  (* No encryption *)
  assert (n land 0b00100000 = 0);
  let tag_type = n land 0b11111 in
  (* Printf.printf "tag: %d\n%!" tag_type; *)
  let data_size = input_u24 ic in
  (* Printf.printf "size: %d\n%!" data_size; *)
  let timestamp = input_u24 ic in
  let timestamp_extended = input_byte ic in
  let timestamp =
    Int32.add
      (Int32.shift_left (Int32.of_int timestamp_extended) 24)
      (Int32.of_int timestamp)
  in
  (* Stream id *)
  assert (input_u24 ic = 0);
  let ans =
    match tag_type with
      | 8 ->
          let data = really_input_string ic data_size in
          `Audio data
      | 9 ->
          let data = really_input_string ic data_size in
          `Video data
      | 18 ->
          let data = really_input_string ic data_size in
          let amf = Amf.decode data |> Array.of_list in
          `Data (Amf.get_string amf.(0), amf.(1))
      | _ -> assert false
  in
  (* Size of previous tag *)
  ignore (input_u32 ic);
  (timestamp, ans)

let read_metadata ic =
  match read_tag ic with
    | _, `Data ("onMetaData", amf) -> amf
    | _ -> assert false
