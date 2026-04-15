open Brr
open Brr_lwd
open Lwd_infix

let models =
  let module P = Params in
  [
    (Moda_masu.title, P.Moda_masu P.Moda_masu.default);
    (Masu.title, P.Masu P.Masu.default);
    (Baggi.title, P.Baggi P.Baggi.default);
    (Corolles.title, P.Corolles P.Corolles.default);
    (Katta_cutters.title, P.Katta_cutters P.Katta_cutters.default);
  ]

let apply_params = function
  | Params.Masu p -> Masu.ui p
  | Moda_masu p -> Moda_masu.ui p
  | Baggi p -> Baggi.ui p
  | Corolles p -> Corolles.ui p
  | Katta_cutters p -> Katta_cutters.ui p

let ui state =
  let model_select model =
    let on_click _ =
      (* Add a history entry only when switching model and not every time the
         state changes. *)
      Url_utils.push ();
      Lwd.set state (apply_params (snd model))
    in
    `R
      (Elwd.a
         ~ev:[ `P (Elwd.handler Ev.click on_click) ]
         [ `P (El.txt' (fst model)) ])
  in
  Elwd.div
    ~at:[ `P (At.class' (Jstr.v "content")) ]
    [
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "model-select")) ]
           (List.map model_select models));
      `R
        (let$* ui_elts, _ = Lwd.get state in
         Elwd.div ui_elts);
    ]

let url state =
  let fragment =
    let$* _, p = Lwd.get state in
    Lwd.map p ~f:Params.encode
  in
  let on_params_change fragment =
    match Params.decode fragment with
    | Some p -> Lwd.set state (apply_params p)
    | None -> ()
  in
  on_params_change (Url_utils.start ~on_change:on_params_change fragment)

let () =
  let state = Lwd.var (apply_params (snd (List.hd models))) in
  url state;
  Brr_lwd_utils.start (ui state)
