module Decoder = struct
  (** Decode parameters in the URL fragment. *)

  let args = ref []

  let rec fold_until acc f = function
    | hd :: tl -> (
        match f hd with Some e -> fold_until (e :: acc) f tl | None -> acc)
    | [] -> acc

  let get ~default f =
    match !args with
    | hd :: tl ->
        args := tl;
        Option.value (f hd) ~default
    | [] -> default

  let string default = get ~default (fun x -> Some x)
  let float default = get ~default float_of_string_opt
  let bool default = get ~default bool_of_string_opt

  let all_remaining f default =
    match !args with
    | [] -> default
    | args' ->
        args := [];
        List.rev (fold_until [] f args')
end

module Masu = struct
  type t = { w : float }

  let default = { w = 100. }

  let decode () =
    let open Decoder in
    let w = float default.w in
    { w }
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

  let decode () =
    let open Decoder in
    let w = float default.w and l = float default.l and h = float default.h in
    let lid = bool default.lid in
    let lid_margin_w = float default.lid_margin_w
    and lid_margin_l = float default.lid_margin_l in
    { w; l; h; lid; lid_margin_w; lid_margin_l }
end

module Baggi = struct
  type t = { w : float; l : float }

  let default = { w = 30.; l = 100. }

  let decode () =
    let open Decoder in
    let w = float default.w and l = float default.l in
    { w; l }
end

module Corolles = struct
  type t = { w : float; l : float; h : float; with_flap : bool }

  let default = { w = 50.; l = 50.; h = 20.; with_flap = false }

  let decode () =
    let open Decoder in
    let w = float default.w and l = float default.l and h = float default.h in
    let with_flap = bool default.with_flap in
    { w; l; h; with_flap }
end

module Katta_cutters = struct
  type t = { w : float; h : float; compartments : float list }

  let default = { w = 60.; h = 30.; compartments = [ 50.; 50. ] }

  let decode () =
    let open Decoder in
    let w = float default.w and h = float default.h in
    let compartments = all_remaining float_of_string_opt default.compartments in
    { w; h; compartments }
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
  function
  | Masu { w } -> spf "masu,%g" w
  | Moda_masu { w; l; h; lid; lid_margin_w; lid_margin_l } ->
      spf "moda-masu,%g,%g,%g,%b,%g,%g" w l h lid lid_margin_w lid_margin_l
  | Baggi { w; l } -> spf "baggi,%g,%g" w l
  | Corolles { w; l; h; with_flap } ->
      spf "corolles,%g,%g,%g,%b" w l h with_flap
  | Katta_cutters { w; h; compartments } ->
      spf "accordion-style-divider,%g,%g,%s" w h
        (String.concat "," (List.map (spf "%g") compartments))

let decode fragment =
  Decoder.args := String.split_on_char ',' fragment;
  match Decoder.string "" with
  | "masu" -> Masu (Masu.decode ())
  | "moda-masu" -> Moda_masu (Moda_masu.decode ())
  | "baggi" -> Baggi (Baggi.decode ())
  | "corolles" -> Corolles (Corolles.decode ())
  | "accordion-style-divider" -> Katta_cutters (Katta_cutters.decode ())
  | _ -> default
