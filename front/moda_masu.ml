open Brr
open Brr_lwd
open Gg
open Vg
open Lwd_infix
open Draw_utils
open Params.Moda_masu

let title = "Moda Masu"
let seq_lift_list l = Lwd_seq.lift (Lwd.pure (Lwd_seq.of_list l))

type t = {
  x_folds : float * float * float;
      (** Coordinate to folds as a distance from the center point. *)
  y_folds : float * float * float;
  paper_size : int * int;
  lidh : float;  (** The height difference between the box and the lid. *)
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
  let label_x ?(below = false) x =
    (* Display a distance from an edge of the diagonal instead of a distance
       from the center point. *)
    let txt = Ui.mm ((diag_len /. 2.) +. x) in
    let x = x *. labels_unit in
    (text_centered ~measure_text txt
    |> I.move (V2.v x (if below then ~-.2. -. font_size else 2.)))
    ++ rect_mid (P2.v x 0.) (Size2.v 0.3 2.) Color.black
  in
  let label_y y =
    (* Invert the scale for aesthetic purposes. *)
    let text = Ui.mm (diag_len -. ((diag_len /. 2.) +. y)) in
    let y = y *. labels_unit in
    label_right 0. y text
  in
  let label_c =
    let text = Ui.mm (diag_len /. 2.) in
    (text_left ~measure_text text
    |> I.move (V2.v ~-.2. (font.Font.size /. ~-.3.)))
    ++ rect_mid (P2.v 0. 0.) (Size2.v 2. 0.3) Color.black
    |> I.rot (Float.pi /. 4.)
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
  ++ labels_x ++ labels_y ++ label_c

let lid_padding = (2., 4.)

let compute { w; l; h; lid; lid_margin_w; lid_margin_l } =
  let lidh = (lid_margin_w +. lid_margin_l) /. 4. in
  let w, l, h =
    if lid then (w +. lid_margin_w, l +. lid_margin_l, h -. lidh) else (w, l, h)
  in
  let paper_w = int_of_float ((w +. l +. (4.0 *. h)) /. Float.sqrt 2.0) in
  let fold dim i = (dim /. 2.) +. (h *. float i) in
  let x_folds = (fold w 0, fold w 1, fold w 2) in
  let y_folds = (fold l 0, fold l 1, fold l 2) in
  { x_folds; y_folds; paper_size = (paper_w, paper_w); lidh }

let ui { w; l; h; lid; lid_margin_w; lid_margin_l } =
  let box_w = Lwd.var w in
  let box_h = Lwd.var h in
  let box_l = Lwd.var l in
  let lid = Lwd.var lid in
  let lid_margin_w = Lwd.var lid_margin_w in
  let lid_margin_l = Lwd.var lid_margin_l in
  let params =
    let$ w = Lwd.get box_w
    and$ l = Lwd.get box_l
    and$ h = Lwd.get box_h
    and$ lid = Lwd.get lid
    and$ lid_margin_w = Lwd.get lid_margin_w
    and$ lid_margin_l = Lwd.get lid_margin_l in
    { w; l; h; lid; lid_margin_w; lid_margin_l }
  in
  let t = Lwd.map ~f:compute params in
  let input_rows =
    [
      `R (Ui.input_row "Box width" (Ui.float_input box_w));
      `R (Ui.input_row "Box length" (Ui.float_input box_l));
      `R (Ui.input_row "Box height" (Ui.float_input box_h));
      `R
        (Ui.input_row "Paper size"
           (let$ { paper_size = w, h; _ } = t in
            El.txt' (Printf.sprintf "%d x %d" w h)));
      `R (Ui.input_row "Lid" (Ui.boolean_input lid));
      `S
        (let$ lid_inputs =
           let lidh_txt =
             let$ { lidh; _ } = t in
             El.txt' (Ui.mm lidh)
           in
           seq_lift_list
             [
               Ui.input_row "Lid margin"
                 (Elwd.div
                    ~at:[ `P (At.class' (Jstr.v "inputs-group")) ]
                    [
                      `R (Ui.float_input lid_margin_w);
                      `P (El.txt' " x ");
                      `R (Ui.float_input lid_margin_l);
                    ]);
               Ui.input_row "Lid height difference" lidh_txt;
             ]
         and$ { lid; _ } = params in
         if lid then lid_inputs else Lwd_seq.empty);
    ]
  in
  let ui =
    Ui.box_ui' title ~input_rows ~image:(image t)
      ~resources:
        [
          ( "Tuto 5 : Les boîte Moda Masu",
            "Les ludistes origamistes",
            "https://www.youtube.com/watch?v=fxZMY6v3big" );
        ]
  in
  ( ui,
    let$ params in
    Params.Moda_masu params )
