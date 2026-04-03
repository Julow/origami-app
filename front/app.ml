open Brr
open Brr_canvas
open Brr_lwd
open Gg
open Vg
open Lwd_infix

let ( ++ ) a b = I.blend b a

let font =
  { Font.name = "sans-serif"; slant = `Normal; weight = `W400; size = 2.5 }

(** Draw text centered around [0] on the [x] axis. *)
let text_centered ~measure_text font color text =
  I.cut_glyphs ~text font [] (I.const color)
  |> I.move (V2.v (~-.(measure_text font text) /. 2.) 0.)

let feuille color =
  let sq = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 50. 50.)) in
  I.const color |> I.cut sq

let diag angle w =
  (* Drawing a rectangle instead of a line with a outline because the canvas
     backend doesn't support cutting an outline on an arbitrary image. *)
  let rect = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 100. w)) in
  let rect = P.tr (M3.rot2 angle) rect in
  I.cut rect (feuille Color.black)

let float_input var =
  let on_input ev =
    let target = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
    let value = El.prop El.Prop.value target in
    Lwd.set var (Jstr.to_float value)
  in
  Elwd.input
    ~at:
      [
        `P (At.type' (Jstr.v "number"));
        `P (At.value (Jstr.of_float (Lwd.peek var)));
      ]
    ~ev:[ `P (Elwd.handler Ev.input on_input) ]
    ()

module Moda_masu = struct
  type t = {
    x_folds : float * float * float;
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
    let labels_unit = view_diag_len /. diag_len in
    let label_x ?(below = false) x txt =
      let x = x *. labels_unit in
      text_centered ~measure_text font Color.black txt
      |> I.move (V2.v x (if below then ~-.2. -. font.Font.size else 2.))
    in
    let label_y y text =
      let y = y *. labels_unit in
      I.cut_glyphs ~text font [] (I.const Color.black) |> I.move (V2.v 2. y)
    in
    let mm = Printf.sprintf "%.0fmm" in
    let labels_x =
      let a, b, c = t.x_folds in
      label_x a (mm a)
      ++ label_x ~below:true b (mm b)
      ++ label_x c (mm c)
      ++ label_x ~-.a (mm a)
      ++ label_x ~below:true ~-.b (mm b)
      ++ label_x ~-.c (mm c)
    in
    let labels_y =
      let a, b, c = t.y_folds in
      label_y a (mm a)
      ++ label_y b (mm b)
      ++ label_y c (mm c)
      ++ label_y ~-.a (mm a)
      ++ label_y ~-.b (mm b)
      ++ label_y ~-.c (mm c)
    in
    ignore t.y_folds;
    (feuille (Color.v_srgb 0.314 0.784 0.471)
     ++ diag (Float.pi /. 4.) 0.3
     ++ diag ~-.(Float.pi /. 4.) 0.3
    |> I.rot (Float.pi /. 4.))
    ++ labels_x ++ labels_y

  let compute w h d =
    let$ w = Lwd.get w and$ h = Lwd.get h and$ d = Lwd.get d in
    let paper_w = int_of_float ((w +. d +. (4.0 *. h)) /. Float.sqrt 2.0) in
    let fold dim i = (dim /. 2.) +. (h *. float i) in
    let x_folds = (fold w 0, fold w 1, fold w 2) in
    let y_folds = (fold d 0, fold d 1, fold d 2) in
    { x_folds; y_folds; paper_size = (paper_w, paper_w) }

  let ui () =
    let box_w = Lwd.var 100. in
    let box_h = Lwd.var 30. in
    let box_d = Lwd.var 50. in
    let t = compute box_w box_h box_d in
    let inputs =
      [
        ("Box width", float_input box_w);
        ("Box height", float_input box_h);
        ("Box depth", float_input box_d);
        ( "Paper size",
          let$ { paper_size = w, h; _ } = t in
          El.txt' (Printf.sprintf "%dmm x %dmm" w h) );
      ]
    in
    (inputs, image t)
end

let canvas_elwd image =
  let el =
    El.canvas ~at:At.[ int (Jstr.v "width") 400; int (Jstr.v "height") 400 ] []
  in
  let vgr = Vg_utils.create (Canvas.of_el el) in
  let measure_text = Vg_utils.measure_text vgr in
  let first_render = ref true in
  let$ image = image ~measure_text in
  if !first_render then (
    (* Delay the first rendering until the canvas is inserted in the document,
       otherwise the canvas would remain blank. *)
    ignore (G.request_animation_frame (fun _ -> Vg_utils.render vgr image));
    first_render := false)
  else Vg_utils.render vgr image;
  el

let ui =
  let inputs, image = Moda_masu.ui () in
  Elwd.div
    ~at:[ `P (At.class' (Jstr.v "content")) ]
    [
      `P (El.h1 [ El.txt' "Moda masu" ]);
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "content-box")) ]
           [
             `R
               (Elwd.table
                  ~at:[ `P (At.class' (Jstr.v "inputs")) ]
                  (List.map
                     (fun (label, elwd) ->
                       `R
                         (Elwd.tr
                            [
                              `P (El.td [ El.txt' label ]);
                              `R (Elwd.td [ `R elwd ]);
                            ]))
                     inputs));
             `R (Elwd.div [ `R (canvas_elwd image) ]);
           ]);
    ]

let () = Lwd_utils.start ui
