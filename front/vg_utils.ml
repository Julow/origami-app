open Vg
open Gg
open Brr_canvas

type t = {
  vgr : Vgr.t;
  size : Size2.t;
  view : Box2.t;
  ctx : C2d.t;  (** 2d context used to measure text. *)
}

let create cv =
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
  let ctx = C2d.get_context cv in
  { vgr; size; view; ctx }

let render t image = ignore (Vgr.render t.vgr (`Image (t.size, t.view, image)))

let measure_text t font text =
  let css = Vgr.Private.Font.css_font ~unit:"px" font in
  C2d.set_font t.ctx (Jstr.v css);
  let metrics = C2d.measure_text t.ctx (Jstr.v text) in
  C2d.Text_metrics.width metrics
