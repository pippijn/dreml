open Ast

let set_begin = 1
let set_end   = 256

let of_array =
  Array.fold_left (fun l -> function
    | None -> l
    | Some c -> c :: l
  ) []


let compress list =
  let chars = Array.make set_end None in
  List.iter (fun c ->
    chars.(Char.code (Sloc.value c)) <- Some c
  ) list;
  of_array chars


let rec make_range range low high =
  if low > high then
    range
  else
    make_range (Char.chr low :: range) (low + 1) high


let chrs_of_range = function
  (* single characters need no resolution *)
  | Single c -> [c]
  (* resolve ranges as list of characters in the range *)
  | Range (low, high) ->
      let range =
        make_range []
          (Char.code (Sloc.value low))
          (Char.code (Sloc.value high))
      in
      List.rev_map (Sloc.at low) range


let chrs_of_ranges list =
  compress (List.concat (List.map chrs_of_range list))


let full_chr_list = chrs_of_range (Range (
  Sloc.generated (Char.chr (set_begin  )),
  Sloc.generated (Char.chr (set_end - 1))))
let empty_chr_list = []


let of_list chars =
  Positive (List.rev_map (fun c -> Single c) chars)


let difference (a : char Sloc.t list) (b : char Sloc.t list) =
  let chars = Array.make set_end None in
  List.iter (fun c ->
    chars.(Char.code (Sloc.value c)) <- Some c
  ) a;
  List.iter (fun c ->
    chars.(Char.code (Sloc.value c)) <- None
  ) b;
  of_array chars


let invert cc =
  difference full_chr_list cc


let to_chr_list = function
  (* resolve positive character classes as list of its characters *)
  | Positive list ->
      chrs_of_ranges list

  (* resolve Negative character classes as the difference between
   * the full range and this class *)
  | Negative list ->
      invert (chrs_of_ranges list)
