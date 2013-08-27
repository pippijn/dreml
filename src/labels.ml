module Int = struct
  type t = int deriving (Show)

  let table : (string, int) Hashtbl.t = Hashtbl.create 10

  let make x =
    try
      Hashtbl.find table x
    with Not_found ->
      let label = Hashtbl.length table + 1 in
      Hashtbl.add table x label;
      label

  let rename = (-) 0
  let to_string = string_of_int

  let unrename x =
    if x < 0 then
      -x
    else
      x
end

module String = struct
  type t = string deriving (Show)

  let make x = x
  let rename = (^) "'"
  let to_string x = x

  let unrename x =
    if x.[0] = '\'' then
      String.sub x 1 (String.length x - 1)
    else
      x
end
