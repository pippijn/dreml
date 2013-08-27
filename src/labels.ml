module Int = struct
  type t = int

  let forward  : (string, int) Hashtbl.t = Hashtbl.create 10
  let backward : (int, string) Hashtbl.t = Hashtbl.create 10

  let make x =
    try
      Hashtbl.find forward x
    with Not_found ->
      let label = Hashtbl.length forward + 1 in
      Hashtbl.add forward  x label;
      Hashtbl.add backward label x;
      label

  let rename = (-) 0

  let unrename x =
    if x < 0 then
      -x
    else
      x

  let to_string x =
    let label = unrename x in
    let name = Hashtbl.find backward label in

    if label <> x then
      "'" ^ name
    else
      name

  module Show_t = Deriving_Show.Defaults(struct
    type a = t

    let format fmt a =
      Show.format<string> fmt (to_string a)
  end)

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
