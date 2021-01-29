(** Operations on FLV files. *)

open IO

let byte n = String.make 1 (char_of_int n)

type t = out_channel * int ref (* size of previous tag *)

let oc (f : t) = fst f

(** Previous tag size. *)
let pts (f : t) = snd f

let open_out ?(audio=true) ?(video=true) fname : t =
  let oc = open_out fname in
  output_string oc "FLV\x01";
  output_string oc (byte ((if audio then 4 else 0) + (if video then 1 else 0)));
  output_string oc (bits_of_int32 (Int32.of_int 9));
  oc, ref 0

let close_out f = close_out (oc f)

let write_tag f tag timestamp data =
  let oc = oc f in
  let s = pts f in
  output_string oc (bits_of_int32 (Int32.of_int !s)); (* size of previous tag *)
  s := 11 + String.length data;
  output_string oc (byte tag);
  output_string oc (bits_of_int24 (String.length data));
  output_string oc (bits_of_int24 (Int32.to_int timestamp)); (* timestamp *)
  (* output_string oc "\0x00"; (\* higher bit of timestamp *\) *)
  (* output_string oc (bits_of_int24 0); (\* stream id, always 0 *\) *)
  output_string oc "\x00\x00\x00\x00";
  output_string oc data

let write_data f l =
  let data = AMF.encode_list l in
  write_tag f 18 Int32.zero data

let write_metadata f l =
  let l = [AMF.String "onMetaData"; AMF.Map l] in
  write_data f l

let write_audio f timestamp data = write_tag f 8 timestamp data

let write_video f timestamp data = write_tag f 9 timestamp data
