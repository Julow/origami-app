open Vg
open Gg
open Brr_canvas

type t = { vgr : Vgr.t; size : Size2.t; view : Box2.t }

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
  { vgr; size; view }

let render t image = ignore (Vgr.render t.vgr (`Image (t.size, t.view, image)))
