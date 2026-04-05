open Brr
open Brr_canvas
open Brr_lwd
open Lwd_infix

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

let models = [ ("Moda Masu", Moda_masu.ui); ("Masu", Masu.ui) ]

let ui =
  let model =
    let n, ui = List.hd models in
    Lwd.var (n, ui ())
  in
  let inputs_table =
    Lwd.join
    @@
    let$ _, (inputs, _) = Lwd.get model in
    Elwd.table
      ~at:[ `P (At.class' (Jstr.v "inputs")) ]
      (List.map
         (fun (label, elwd) ->
           `R
             (Elwd.tr
                [ `P (El.td [ El.txt' label ]); `R (Elwd.td [ `R elwd ]) ]))
         inputs)
  in
  let model_select (name, ui) =
    let on_click _ = Lwd.set model (name, ui ()) in
    `R
      (Elwd.a ~ev:[ `P (Elwd.handler Ev.click on_click) ] [ `P (El.txt' name) ])
  in
  Elwd.div
    [
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "content model-select")) ]
           (List.map model_select models));
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "content")) ]
           [
             `R
               (Elwd.h1
                  [
                    `R
                      (let$ name, _ = Lwd.get model in
                       El.txt' name);
                  ]);
             `R
               (Elwd.div
                  ~at:[ `P (At.class' (Jstr.v "content-box")) ]
                  [
                    `R inputs_table;
                    `R
                      (Elwd.div
                         [
                           `R
                             (Lwd.join
                             @@ let$ _, (_, image) = Lwd.get model in
                                canvas_elwd image);
                         ]);
                  ]);
           ]);
    ]

let () = Lwd_utils.start ui
