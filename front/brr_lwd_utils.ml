open Brr

(** From Lwd [focustest-brr] example. *)
let start ui =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    ignore (G.request_animation_frame (fun _ -> ignore (Lwd.quick_sample ui)))
  in
  let on_load _ =
    El.append_children (Document.body G.document) [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  ignore (Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window))
