open Brr
open Vg
open Lwd_infix
open Draw_utils

let title = "Corolles"

type t = {
  x_folds : float list;
  y_folds : float list;
  paper_w : int;
  paper_h : int;
}

let image t ~measure_text =
  let$ t = t in
  let fw, fh =
    let pw = float t.paper_w and ph = float t.paper_h in
    if pw < ph then (50. *. pw /. ph, 50.) else (50., 50. *. ph /. pw)
  in
  let fx, fy = (fw /. -2., fh /. -2.) in
  let x_unit = fw /. float t.paper_w and y_unit = fh /. float t.paper_h in
  let labels_x =
    List.fold_left
      (fun acc fold ->
        let text = Printf.sprintf "%.0fmm" fold in
        acc
        ++ label_left ~measure_text ~rot_line:(Float.pi /. -2.)
             ~rot_text:(Float.pi /. -4.)
             (fx +. (fold *. x_unit))
             ~-.fy text)
      I.void t.x_folds
  in
  let labels_y =
    List.fold_left
      (fun acc fold ->
        let text = Printf.sprintf "%.0fmm" fold in
        acc
        ++ label_right ~rot_text:(Float.pi /. -4.) ~-.fx
             (fy +. (fold *. y_unit))
             text)
      I.void t.y_folds
  in
  feuille ~w:fw ~h:fh () ++ labels_x ++ labels_y |> I.rot (Float.pi /. 4.)

let ui () =
  let box_w = Lwd.var 50. in
  let box_l = Lwd.var 50. in
  let box_h = Lwd.var 20. in
  let with_flap = Lwd.var false in
  let t =
    let$ w = Lwd.get box_w
    and$ l = Lwd.get box_l
    and$ h = Lwd.get box_h
    and$ with_flap = Lwd.get with_flap in
    let x_folds = [ h; h +. w ] in
    let y_folds =
      [ h; h +. l ] @ if with_flap then [ (h *. 2.) +. l ] else []
    in
    {
      x_folds;
      y_folds;
      paper_w = int_of_float (w +. (h *. 2.));
      paper_h = int_of_float (l +. (h *. 2.) +. if with_flap then h else 0.);
    }
  in
  let paper_size_txt =
    let$ t = t in
    El.txt' (Printf.sprintf "%dmm x %dmm" t.paper_w t.paper_h)
  in
  let inputs =
    [
      ("Box width", Ui.float_input box_w);
      ("Box length", Ui.float_input box_l);
      ("Box height", Ui.float_input box_h);
      ("With flap", Ui.boolean_input with_flap);
      ("Paper size", paper_size_txt);
    ]
  in
  Ui.box_ui ~inputs ~image:(image t)
  @ [
      Ui.resources
        [
          ( "Tuto Origami 12 : Les boîtes corolles",
            "Mémo-règles Jeux de plateau",
            "https://www.youtube.com/watch?v=8EJCqO5RVgU" );
        ];
    ]
