let error s = failwith @@ Format.sprintf "%s" s

let usage = Format.sprintf "Usage: %s <size_of_the_map : int>" Sys.argv.(0)

let height, width =
  if Array.length Sys.argv <> 2 then error usage
  else
    match int_of_string_opt Sys.argv.(1) with
    | None -> error usage
    | Some size -> (size, size)

let world = Array.make_matrix width height false

module Pp : sig
  val world : Format.formatter -> unit -> unit
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

  let world fmt () =
    pp_array
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      fmt print_boules world
end

module Life_and_death : sig
  val update : unit -> unit
end = struct
  let count_live_neighbours (posx, posy) =
    let neighbours =
      [ (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1) ]
    in
    List.fold_left
      (fun live_ones (n_posx, n_posy) ->
        let coordx = posx + n_posx in
        let coordy = posy + n_posy in
        if
          coordx < 0
          || coordx >= width - 1
          || coordy < 0
          || coordy >= height - 1
          || not world.(coordx).(coordy)
        then live_ones
        else live_ones + 1 )
      0 neighbours

  let update () =
    let live_neighbours =
      Array.mapi
        (fun j row ->
          Array.mapi (fun i _cell -> count_live_neighbours (i, j)) row )
        world
    in
    for j = 0 to height - 1 do
      for i = 0 to width - 1 do
        world.(i).(j) <-
          ( if world.(i).(j) then
              live_neighbours.(i).(j) = 2 || live_neighbours.(i).(j) = 3
            else live_neighbours.(i).(j) = 3 )
      done
    done
end

let observe () =
  let buf = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a" Pp.world ();
  Format.printf "\027[2J%s" (Buffer.contents buf);
  Format.pp_print_flush Format.std_formatter ()

let rec over_time () =
  observe ();
  Life_and_death.update ();
  over_time ()

let jdlvdm () =
  if
    width < 0 || height < 0
    || width > Sys.max_array_length
    || height > Sys.max_array_length
  then error "World size cannot be too big or negative"
  else
    Array.iteri
      (fun j r ->
        Array.iteri
          (fun i _b ->
            let r = Random.int 100 in
            world.(i).(j) <- r >= 50 )
          r )
      world;
  over_time ()

let () =
  Random.self_init ();
  jdlvdm ()
