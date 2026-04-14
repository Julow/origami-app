module Masu = struct
  type t = { w : float }

  let default = { w = 100. }
end

module Moda_masu = struct
  type t = { w : float; l : float; h : float; lid : bool }

  let default = { w = 100.; l = 50.; h = 30.; lid = false }
end

module Baggi = struct
  type t = { w : float; l : float }

  let default = { w = 30.; l = 100. }
end

module Corolles = struct
  type t = { w : float; l : float; h : float; with_flap : bool }

  let default = { w = 50.; l = 50.; h = 20.; with_flap = false }
end

module Katta_cutters = struct
  type t = { w : float; h : float; compartments : float list }

  let default = { w = 60.; h = 30.; compartments = [ 50.; 50. ] }
end

type t =
  | Masu of Masu.t
  | Moda_masu of Moda_masu.t
  | Baggi of Baggi.t
  | Corolles of Corolles.t
  | Katta_cutters of Katta_cutters.t

let encode =
  let spf = Printf.sprintf in
  let float v = Printf.sprintf "%g" v in
  function
  | Masu { w } -> spf "masu,%g" w
  | Moda_masu { w; l; h; lid } -> spf "moda-masu,%g,%g,%g,%b" w l h lid
  | Baggi { w; l } -> spf "baggi,%g,%g" w l
  | Corolles { w; l; h; with_flap } ->
      spf "corolles,%g,%g,%g,%b" w l h with_flap
  | Katta_cutters { w; h; compartments } ->
      spf "accordion-style-divider,%g,%g,%s" w h
        (String.concat "," (List.map float compartments))

let decode fragment =
  let ( let+ ) x f = Option.map f x in
  let ( and+ ) a b =
    match (a, b) with Some a, Some b -> Some (a, b) | _ -> None
  in
  let float = float_of_string_opt in
  let bool = bool_of_string_opt in
  let rec list acc f = function
    | hd :: tl -> (
        match f hd with Some e -> list (e :: acc) f tl | None -> None)
    | [] -> Some (List.rev acc)
  in
  match String.split_on_char ',' fragment with
  | [ "masu"; w ] ->
      let+ w = float w in
      Masu { w }
  | [ "moda-masu"; w; l; h; lid ] ->
      let+ w = float w and+ l = float l and+ h = float h and+ lid = bool lid in
      Moda_masu { w; l; h; lid }
  | [ "baggi"; w; l ] ->
      let+ w = float w and+ l = float l in
      Baggi { w; l }
  | [ "corolles"; w; l; h; with_flap ] ->
      let+ w = float w
      and+ l = float l
      and+ h = float h
      and+ with_flap = bool with_flap in
      Corolles { w; l; h; with_flap }
  | "accordion-style-divider" :: w :: h :: (_ :: _ :: _ as compartments) ->
      let+ w = float w
      and+ h = float h
      and+ compartments = list [] float compartments in
      Katta_cutters { w; h; compartments }
  | _ -> None
