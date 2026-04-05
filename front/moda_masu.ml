open Brr
open Gg
open Vg
open Lwd_infix
open Ui

type t = {
  x_folds : float * float * float;
      (** Coordinate to folds as a distance from the center point. *)
  y_folds : float * float * float;
  paper_size : int * int;
}

let image t ~measure_text =
  let view_diag_len = Float.sqrt (50. *. 50. *. 2.) in
  let$ t = t in
  let diag_len =
    let w, h = t.paper_size in
    Float.sqrt (float (w * h * 2))
  in
  (* Convert from paper coordinate to image coordinate. *)
  let labels_unit = view_diag_len /. diag_len in
  let mm v = Printf.sprintf "%.0fmm" (Float.round v) in
  let label_x ?(below = false) x =
    (* Display a distance from an edge of the diagonal instead of a distance
       from the center point. *)
    let txt = mm ((diag_len /. 2.) +. x) in
    let x = x *. labels_unit in
    (text_centered ~measure_text font Color.black txt
    |> I.move (V2.v x (if below then ~-.2. -. font.Font.size else 2.)))
    ++ rect_mid (P2.v x 0.) (Size2.v 0.3 2.) Color.black
  in
  let label_y y =
    (* Invert the scale for aesthetic purposes. *)
    let text = mm (diag_len -. ((diag_len /. 2.) +. y)) in
    let y = y *. labels_unit in
    label_right 0. y text
  in
  let labels_x =
    let a, b, c = t.x_folds in
    label_x a ++ label_x ~below:true b ++ label_x c ++ label_x ~-.a
    ++ label_x ~below:true ~-.b ++ label_x ~-.c
  in
  let labels_y =
    let a, b, c = t.y_folds in
    label_y a ++ label_y b ++ label_y c ++ label_y ~-.a ++ label_y ~-.b
    ++ label_y ~-.c
  in
  (feuille () ++ diag (Float.pi /. 4.) 0.3 ++ diag ~-.(Float.pi /. 4.) 0.3
  |> I.rot (Float.pi /. 4.))
  ++ labels_x ++ labels_y

let compute w h l =
  let$ w = Lwd.get w and$ h = Lwd.get h and$ l = Lwd.get l in
  let paper_w = int_of_float ((w +. l +. (4.0 *. h)) /. Float.sqrt 2.0) in
  let fold dim i = (dim /. 2.) +. (h *. float i) in
  let x_folds = (fold w 0, fold w 1, fold w 2) in
  let y_folds = (fold l 0, fold l 1, fold l 2) in
  { x_folds; y_folds; paper_size = (paper_w, paper_w) }

let ui () =
  let box_w = Lwd.var 100. in
  let box_h = Lwd.var 30. in
  let box_l = Lwd.var 50. in
  let t = compute box_w box_h box_l in
  let inputs =
    [
      ("Box width", float_input box_w);
      ("Box length", float_input box_l);
      ("Box height", float_input box_h);
      ( "Paper size",
        let$ { paper_size = w, h; _ } = t in
        El.txt' (Printf.sprintf "%dmm x %dmm" w h) );
    ]
  in
  (inputs, image t)
