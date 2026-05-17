module Masu = struct
  type t = { w : float }

  let default = { w = 100. }
end

module Moda_masu = struct
  type t = {
    w : float;
    l : float;
    h : float;
    lid : bool;
    lid_margin_w : float;
    lid_margin_l : float;
  }

  let default =
    {
      w = 100.;
      l = 50.;
      h = 30.;
      lid = false;
      lid_margin_w = 4.;
      lid_margin_l = 4.;
    }
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

let default = Moda_masu Moda_masu.default

let encode =
  let spf = Printf.sprintf in
  let float v = Printf.sprintf "%g" v in
  function
  | Masu { w } -> spf "masu,%g" w
  | Moda_masu { w; l; h; lid; lid_margin_w; lid_margin_l } ->
      spf "moda-masu,%g,%g,%g,%b,%g,%g" w l h lid lid_margin_w lid_margin_l
  | Baggi { w; l } -> spf "baggi,%g,%g" w l
  | Corolles { w; l; h; with_flap } ->
      spf "corolles,%g,%g,%g,%b" w l h with_flap
  | Katta_cutters { w; h; compartments } ->
      spf "accordion-style-divider,%g,%g,%s" w h
        (String.concat "," (List.map float compartments))

let rec fold_until acc f = function
  | hd :: tl -> (
      match f hd with Some e -> fold_until (e :: acc) f tl | None -> acc)
  | [] -> acc

let decode fragment =
  let args = ref (String.split_on_char ',' fragment) in
  let get ~default f =
    match !args with
    | hd :: tl ->
        args := tl;
        Option.value (f hd) ~default
    | [] -> default
  in
  let string default = get ~default (fun x -> Some x) in
  let float default = get ~default float_of_string_opt in
  let bool default = get ~default bool_of_string_opt in
  let list default f =
    match !args with
    | [] -> default
    | args' ->
        args := [];
        List.rev (fold_until [] f args')
  in
  match string "" with
  | "masu" ->
      let w = float Masu.default.w in
      Masu { w }
  | "moda-masu" ->
      let d = Moda_masu.default in
      let w = float d.w and l = float d.l and h = float d.h in
      let lid = bool d.lid in
      let lid_margin_w = float d.lid_margin_w
      and lid_margin_l = float d.lid_margin_l in
      Moda_masu { w; l; h; lid; lid_margin_w; lid_margin_l }
  | "baggi" ->
      let d = Baggi.default in
      let w = float d.w and l = float d.l in
      Baggi { w; l }
  | "corolles" ->
      let d = Corolles.default in
      let w = float d.w and l = float d.l and h = float d.h in
      let with_flap = bool d.with_flap in
      Corolles { w; l; h; with_flap }
  | "accordion-style-divider" ->
      let d = Katta_cutters.default in
      let w = float d.w and h = float d.h in
      let compartments = list d.compartments float_of_string_opt in
      Katta_cutters { w; h; compartments }
  | _ -> default
