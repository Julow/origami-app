open Brr
open Brr_lwd

let models =
  [
    Moda_masu.(title, ui);
    Masu.(title, ui);
    Baggi.(title, ui);
    Corolles.(title, ui);
    Katta_cutters.(title, ui);
  ]

let ui =
  let box_ui = Lwd.var (snd (List.hd models) ()) in
  let model_select (title, ui) =
    let on_click _ = Lwd.set box_ui (ui ()) in
    `R
      (Elwd.a
         ~ev:[ `P (Elwd.handler Ev.click on_click) ]
         [ `P (El.txt' title) ])
  in
  Elwd.div
    ~at:[ `P (At.class' (Jstr.v "content")) ]
    [
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "model-select")) ]
           (List.map model_select models));
      `R (Lwd.bind (Lwd.get box_ui) ~f:Elwd.div);
    ]

let () = Brr_lwd_utils.start ui
