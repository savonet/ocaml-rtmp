type t = Number of float | String of string | Object of (string * t) list

let rec encode data =
  let ans = ref [] in
  let push s = ans := s :: !ans in
  let byte n =
    assert (0 <= n && n <= 0xff);
    push (String.make 1 (char_of_int n)) in
  let u16 n =
    assert (0 <= n && n <= 0xffff);
    byte (n lsr 8);
    byte (n land 0xff)
  in
  let u64 n =
    for i = 0 to 7 do
      byte (Int64.to_int (Int64.logand (Int64.shift_right n ((7-i)*8)) (Int64.of_int 0xff)));
    done
  in
  let string s =
    u16 (String.length s);
    push s
  in
  (
    match data with
    | Number x ->
      byte 0x00;
      u64 (Int64.bits_of_float x)
    | String s ->
      byte 0x02;
      string s
    | Object l ->
      byte 0x03;
      List.iter (fun (l,v) -> string l; push (encode v)) l;
      string "";
      byte 0x09
  );
  String.concat "" (List.rev !ans)

let encode_list data =
  String.concat "" (List.map encode data)

let decode data =
  let i = ref 0 in
  let n = String.length data in
  let byte () =
    let x = int_of_char data.[!i] in
    i := !i + 1;
    x
  in
  let u16 () =
    let b1 = byte () in
    let b0 = byte () in
    (b1 lsl 8) + b0
  in
  let u64 () =
    let n = ref Int64.zero in
    for _ = 0 to 7 do
      n := Int64.add (Int64.shift_left !n 8) (Int64.of_int (byte ()))
    done;
    !n
  in
  let string () =
    let len = u16 () in
    let s = String.sub data !i len in
    i := !i + len;
    s
  in
  let double () = Int64.float_of_bits (u64 ()) in
  let rec value () =
    match byte () with
    | 0x00 ->
      (* number *)
      let n = double () in
      Number n
    | 0x02 ->
      (* string *)
      let s = string () in
      String s
    | 0x03 ->
      (* object *)
      let ans = ref [] in
      (* Printf.printf "object: %s\n%!" (string ()); *)
      let l = ref (string ()) in
      Printf.printf "label: %s\n%!" !l;
      while !l <> "" do
        let v = value () in
        ans := (!l, v) :: !ans;
        l := string ()
      done;
      assert (byte () = 0x09);
      Object (List.rev !ans)
    | b ->
      Printf.printf "***** Unknown AMF 0x%02x\n%!" b;
      raise Exit
  in
  let ans = ref [] in
  ( try
      while !i <> n do
        ans := value () :: !ans
      done
    with Exit -> () );
  List.rev !ans

(** String representation of a value (for debugging purposes). *)
let rec to_string = function
  | Number n -> Printf.sprintf "%f" n
  | String s -> "\"" ^ s ^ "\""
  | Object l ->
    let l =
      List.map (fun (l, v) -> l ^ ": " ^ to_string v) l
      |> String.concat ", "
    in
    "{" ^ l ^ "}"

let list_to_string l = String.concat ", " (List.map to_string l)
