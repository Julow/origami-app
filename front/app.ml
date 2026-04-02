open Brr
open Brr_canvas
open Gg
open Vg

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
  fun image ->
    ignore (Vgr.render vgr (`Image (size, view, image)));
    ignore (Vgr.render vgr `End)

let canvas_feuille () =
  Document.find_el_by_id G.document (Jstr.v "canvas-feuille")
  |> Option.get |> Canvas.of_el

let image =
  let feuille color =
    let sq = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 50. 50.)) in
    I.const color |> I.cut sq
  in
  let diag angle w =
    (* Drawing a rectangle instead of a line with a outline because the canvas
       backend doesn't support cutting an outline on an arbitrary image. *)
    let rect = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 100. w)) in
    let rect = P.tr (M3.rot2 angle) rect in
    I.cut rect (feuille Color.black)
  in
  feuille (Color.v_srgb 0.314 0.784 0.471)
  |> I.blend (diag (Float.pi /. 4.) 0.3)
  |> I.blend (diag ~-.(Float.pi /. 4.) 0.3)
  |> I.rot (Float.pi /. 4.)

let () =
  let render = renderer (canvas_feuille ()) in
  render image
