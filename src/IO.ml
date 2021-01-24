(** I/O helper functions. *)

let hex_of_string s =
  let ans = ref "" in
  for i = 0 to String.length s - 1 do
    ans := Printf.sprintf "%s%02x " !ans (int_of_char s.[i])
  done;
  !ans

let write f s =
  (* Printf.printf "write: %s\n%!" (hex_of_string (Bytes.unsafe_to_string s)); *)
  let n = Bytes.length s in
  assert (Unix.write f s 0 n = n)

let write_byte f n =
  assert (0 <= n && n <= 0xff);
  let s = Bytes.create 1 in
  Bytes.set s 0 (char_of_int n);
  write f s

let write_short f n =
  assert (0 <= n && n <= 0xffff);
  let s = Bytes.create 2 in
  Bytes.set s 0 (char_of_int ((n lsr 8) land 0xff));
  Bytes.set s 1 (char_of_int (n land 0xff));
  write f s

let write_int24 f n =
  let s = Bytes.create 3 in
  for i = 0 to 2 do
    Bytes.set s (2 - i) (char_of_int ((n lsr (8 * i)) land 0xff))
  done;
  write f s

let write_int32 f n =
  let s = Bytes.create 4 in
  for i = 0 to 3 do
    Bytes.set s (3 - i)
      (char_of_int
         (Int32.to_int
            (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n (8 * i)))))
  done;
  write f s

let write_int32_le f n =
  let s = Bytes.create 4 in
  for i = 0 to 3 do
    Bytes.set s i
      (char_of_int
         (Int32.to_int
            (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n (8 * i)))))
  done;
  write f s

let read f n =
  let s = Bytes.create n in
  let r = ref 0 in
  while !r < n do
    let r' = Unix.read f s !r (n - !r) in
    assert (r' > 0);
    r := !r + r'
  done;
  s

let read_string f n = Bytes.unsafe_to_string (read f n)
let read_byte f = int_of_char (read_string f 1).[0]

let read_short f =
  let n = read_byte f in
  (n lsl 8) + read_byte f

let read_int24 f =
  let n = read_byte f in
  let n = (n lsl 8) + read_byte f in
  (n lsl 8) + read_byte f

let read_int32 f =
  let s = read_string f 4 in
  let ans = ref Int32.zero in
  for i = 0 to 3 do
    let n = int_of_char s.[3 - i] in
    let n = Int32.of_int n in
    let n = Int32.shift_left n (i * 8) in
    ans := Int32.add !ans n
  done;
  !ans

let read_int32_le f =
  let s = read_string f 4 in
  let ans = ref Int32.zero in
  for i = 0 to 3 do
    let n = int_of_char s.[i] in
    let n = Int32.of_int n in
    let n = Int32.shift_left n (i * 8) in
    ans := Int32.add !ans n
  done;
  !ans

(** Conversion from/to bits. *)

(* All encodings are big endian unless otherwise specified. *)

let bits_of_int32 n =
  let s = Bytes.create 4 in
  let set i k =
    Bytes.set s i
      (char_of_int
         (Int32.to_int
            (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n (8 * k)))))
  in
  set 0 3;
  set 1 2;
  set 2 1;
  set 3 0;
  Bytes.unsafe_to_string s

let int32_of_bits s =
  assert (String.length s = 4);
  let ans = ref Int64.zero in
  for i = 0 to 3 do
    let n = Int64.of_int (int_of_char (s.[i])) in
    ans := Int64.add (Int64.shift_left !ans 8) n
  done;
  !ans

let int64_of_bits s =
  assert (String.length s = 8);
  let ans = ref Int64.zero in
  for i = 0 to 7 do
    let n = Int64.of_int (int_of_char (s.[i])) in
    ans := Int64.add (Int64.shift_left !ans 8) n
  done;
  !ans
