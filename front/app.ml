open Brr
open Brr_lwd
open Lwd_infix

let models =
  [
    Moda_masu.(title, ui);
    Masu.(title, ui);
    Baggi.(title, ui);
    Corolles.(title, ui);
  ]

let ui =
  let model = Lwd.var (List.hd models) in
  let model_select ((title, _) as m) =
    let on_click _ = Lwd.set model m in
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
      `R
        (let$* _, ui = Lwd.get model in
         Elwd.div
           (`R
              (Elwd.h1
                 [
                   `R
                     (let$ title, _ = Lwd.get model in
                      El.txt' title);
                 ])
           :: ui ()));
    ]

let () = Brr_lwd_utils.start ui
