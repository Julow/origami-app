open Brr
open Brr_canvas
open Brr_lwd
open Gg
open Vg
open Lwd_infix

(* Setup [Vgr_htmlc] for a canvas with a size specified with its [width] and
   [height] attributes. The viewport is centered on [0, 0] with a height of
   [100mm] and a width depending on the canvas aspect ratio. *)
let renderer cv =
  let w = float (Canvas.w cv) and h = float (Canvas.h cv) in
  let () =
    (* [Vgr] resizes the canvas according to the device pixel density. Scale
       back the canvas using CSS so it appears with the specified size. *)
    let open Brr.El in
    let el = Canvas.to_el cv in
    set_inline_style Style.width Jstr.(of_float w + v "px") el;
    set_inline_style Style.height Jstr.(of_float h + v "px") el
  in
  let aspect = w /. h in
  let size = Size2.v (100. *. aspect) 100. in
  let view =
    Box2.v (P2.v (~-.50. *. aspect) ~-.50.) (Size2.v (100. *. aspect) 100.)
  in
  let vgr = Vgr.create (Vgr_htmlc.target ~resize:false cv) `Other in
  fun image -> ignore (Vgr.render vgr (`Image (size, view, image)))

let feuille color =
  let sq = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 50. 50.)) in
  I.const color |> I.cut sq

let diag angle w =
  (* Drawing a rectangle instead of a line with a outline because the canvas
     backend doesn't support cutting an outline on an arbitrary image. *)
  let rect = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 100. w)) in
  let rect = P.tr (M3.rot2 angle) rect in
  I.cut rect (feuille Color.black)

let make_image result_text =
  let font =
    { Font.name = "sans-serif"; slant = `Normal; weight = `W400; size = 3. }
  in
  let label =
    I.cut_glyphs ~text:result_text font [] (I.const Color.black)
    |> I.move (V2.v 0. 2.)
  in
  feuille (Color.v_srgb 0.314 0.784 0.471)
  |> I.blend (diag (Float.pi /. 4.) 0.3)
  |> I.blend (diag ~-.(Float.pi /. 4.) 0.3)
  |> I.rot (Float.pi /. 4.)
  |> I.blend label

let canvas_feuille paper_size =
  let el =
    El.canvas ~at:At.[ int (Jstr.v "width") 400; int (Jstr.v "height") 400 ] []
  in
  let render = renderer (Canvas.of_el el) in
  let first_render = ref true in
  let render_state st =
    let img = make_image st in
    (* Delay the first rendering until the canvas is inserted in the
       document, otherwise the canvas would remain blank. *)
    if !first_render then (
      ignore (G.request_animation_frame (fun _ -> render img));
      first_render := false)
    else render img
  in
  let$ paper_size = paper_size in
  render_state paper_size;
  el

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

let paper_size w h d =
  let$ w = Lwd.get w and$ h = Lwd.get h and$ d = Lwd.get d in
  (w +. d +. (4.0 *. h)) /. Float.sqrt 2.0

let ui =
  let box_w = Lwd.var 100. in
  let box_h = Lwd.var 30. in
  let box_d = Lwd.var 50. in
  let paper_size =
    let$ s = paper_size box_w box_h box_d in
    Printf.sprintf "%.1f x %.1f" s s
  in
  let canvas_el = canvas_feuille paper_size in
  Elwd.div
    ~at:[ `P (At.class' (Jstr.v "content")) ]
    [
      `P (El.h1 [ El.txt' "Moda masu" ]);
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "content-box")) ]
           [
             `R
               (let r label elwd =
                  `R
                    (Elwd.tr
                       [
                         `P (El.td [ El.txt' label ]); `R (Elwd.td [ `R elwd ]);
                       ])
                in
                Elwd.table
                  ~at:[ `P (At.class' (Jstr.v "inputs")) ]
                  [
                    r "Box width" (float_input box_w);
                    r "Box height" (float_input box_h);
                    r "Box depth" (float_input box_d);
                    r "Paper size"
                      (let$ paper_size = paper_size in
                       El.txt' paper_size);
                  ]);
             `R (Elwd.div [ `R canvas_el ]);
           ]);
    ]

let () = Lwd_utils.start ui
