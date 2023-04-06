module Pp : sig
  val world : Format.formatter -> bool array array -> unit
end = struct
  let pp_print_iter ~pp_sep iter pp_v ppf v =
    iter
      (fun v ->
        pp_v ppf v;
        pp_sep ppf () )
      v

  let pp_array ~pp_sep fmt ppv arr =
    pp_print_iter ~pp_sep Array.iter ppv fmt arr

  let print_boules fmt arr =
    pp_array
      ~pp_sep:(fun _fmt () -> ())
      fmt
      (fun fmt b -> Format.pp_print_string fmt (if b then "😒" else "🫥"))
      arr

  let world fmt arr =
    pp_array
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      fmt print_boules arr
end

module Life_and_death : sig
  val update : bool array array -> bool array array
end = struct
  type verdict =
    | Death
    | Life

  module Look : sig
    val for_life_or_death : int * int -> int * int -> (int * int) list
  end = struct
    let left (x, y) _max_xy = if x = 0 then None else Some (x - 1, y)

    let up_left (x, y) _max_xy =
      if x = 0 || y = 0 then None else Some (x - 1, y - 1)

    let up (x, y) _max_xy = if y = 0 then None else Some (x, y - 1)

    let up_right (x, y) (max_x, _max_y) =
      if x = max_x - 1 || y = 0 then None else Some (x + 1, y - 1)

    let right (x, y) (max_x, _max_y) =
      if x = max_x - 1 then None else Some (x + 1, y)

    let down_right (x, y) (max_x, max_y) =
      if x = max_x - 1 || y = max_y - 1 then None else Some (x + 1, y + 1)

    let down (x, y) (_max_x, max_y) =
      if y = max_y - 1 then None else Some (x, y + 1)

    let down_left (x, y) (_max_x, max_y) =
      if x = 0 || y = max_y - 1 then None else Some (x - 1, y + 1)

    let movements =
      [ up; up_right; right; down_right; down; down_left; left; up_left ]

    let for_life_or_death pos size =
      List.filter_map (fun f -> f pos size) movements
  end

  let count_live neighbors world =
    List.fold_left
      (fun live (x, y) -> if world.(x).(y) then succ live else live)
      0 neighbors

  let should_it_live_or_die ((posx, posy) as location) world : verdict =
    let size = (Array.length world.(0), Array.length world) in
    let cell = world.(posx).(posy) in
    let neighbors_in = Look.for_life_or_death location size in
    let fate =
      if cell then
        match count_live neighbors_in world with 2 | 3 -> Life | _n -> Death
      else match count_live neighbors_in world with 3 -> Life | _n -> Death
    in
    match fate with
    | Life -> Life
    | Death -> if Random.int 1000 = 0 then Life else Death

  let update world =
    let deep_copy_of arr = Array.map (Array.map Fun.id) arr in
    let new_world = deep_copy_of world in
    Array.iteri
      (fun j l ->
        Array.iteri
          (fun i _c ->
            match should_it_live_or_die (i, j) world with
            | Death -> new_world.(i).(j) <- false
            | Life -> new_world.(i).(j) <- true )
          l )
      world;
    new_world
end

let observe w =
  let buf = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a" Pp.world w;
  Format.printf "\027[2J%s" (Buffer.contents buf);
  Format.pp_print_flush Format.std_formatter ()

let rec over_time world =
  let new_world = Life_and_death.update world in
  observe world;
  new_world |> over_time

let error s = failwith @@ Format.sprintf "%s" s

let jdlv size =
  if size < 0 || size > Sys.max_array_length then
    error "World size cannot be too big or negative"
  else
    let world =
      Array.init size (fun _i ->
          Array.init size (fun _j ->
              let r = Random.int 100 in
              r >= 50 ) )
    in
    world |> over_time

let usage = Format.sprintf "Usage: %s <size_of_the_map : int>" Sys.argv.(0)

let () =
  Random.self_init ();
  if Array.length Sys.argv <> 2 then error usage
  else
    match int_of_string_opt Sys.argv.(1) with
    | None -> error usage
    | Some size -> jdlv size
