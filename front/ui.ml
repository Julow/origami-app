open Brr
open Brr_canvas
open Brr_lwd
open Lwd_infix

let txtf fmt = Printf.ksprintf El.txt' fmt
let mm f = string_of_int Float.(to_int (round f)) ^ "mm"

let float_input' set init =
  let on_input ev =
    let target = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
    let value = El.prop El.Prop.value target in
    set (Jstr.to_float value)
  in
  Elwd.input
    ~at:[ `P (At.type' (Jstr.v "number")); `P (At.value (Jstr.of_float init)) ]
    ~ev:[ `P (Elwd.handler Ev.input on_input) ]
    ()

let float_input var = float_input' (Lwd.set var) (Lwd.peek var)

let boolean_input var =
  let on_change ev =
    let target = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
    let checked = El.prop El.Prop.checked target in
    Lwd.set var checked
  in
  Elwd.input
    ~at:
      [
        `P (At.type' (Jstr.v "checkbox")); `P (At.if' (Lwd.peek var) At.checked);
      ]
    ~ev:[ `P (Elwd.handler Ev.input on_change) ]
    ()

let button ?at label handler =
  Elwd.button ?at
    ~ev:[ `P (Elwd.handler Ev.click handler) ]
    [ `P (El.txt' label) ]

let canvas_elwd image =
  let el =
    El.canvas ~at:At.[ int (Jstr.v "width") 400; int (Jstr.v "height") 400 ] []
  in
  let vgr = Vgr_utils.create (Canvas.of_el el) in
  let measure_text = Vgr_utils.measure_text vgr in
  let first_render = ref true in
  let$ image = image ~measure_text in
  if !first_render then (
    (* Delay the first rendering until the canvas is inserted in the document,
       otherwise the canvas would remain blank. *)
    ignore (G.request_animation_frame (fun _ -> Vgr_utils.render vgr image));
    first_render := false)
  else Vgr_utils.render vgr image;
  el

let input_row ?(extra_cols = []) label input =
  Elwd.tr
    (`P (El.td [ El.txt' label ]) :: `R (Elwd.td [ `R input ]) :: extra_cols)

let box_ui' ~input_rows ~image =
  let inputs_table =
    Elwd.table ~at:[ `P (At.class' (Jstr.v "inputs")) ] input_rows
  in
  [
    `R
      (Elwd.div
         [
           `R
             (Elwd.div
                ~at:[ `P (At.class' (Jstr.v "content-box borders")) ]
                [ `R inputs_table; `R (Elwd.div [ `R (canvas_elwd image) ]) ]);
         ]);
  ]

let box_ui ~inputs ~image =
  let input_rows =
    List.map (fun (label, input) -> `R (input_row label input)) inputs
  in
  box_ui' ~input_rows ~image

let resources rs =
  `P
    (El.div
       ~at:[ At.class' (Jstr.v "borders") ]
       (El.h3 [ El.txt' "Resources" ]
       :: List.map
            (fun (title, author, link) ->
              El.a
                ~at:
                  [
                    At.class' (Jstr.v "resource");
                    At.href (Jstr.v link);
                    At.v (Jstr.v "target") (Jstr.v "_blank");
                  ]
                [
                  El.txt' title;
                  El.span
                    ~at:[ At.class' (Jstr.v "resource-author") ]
                    [ El.txt' author ];
                ])
            rs))
