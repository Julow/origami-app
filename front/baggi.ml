open Brr
open Vg
open Lwd_infix

let title = "Baggi"

let paper_size box_w box_l =
  let$ w = Lwd.get box_w and$ l = Lwd.get box_l in
  (int_of_float (4. *. w), int_of_float (l +. (2. *. w)))

let ui { Params.Baggi.w; l } =
  let box_w = Lwd.var w in
  let box_l = Lwd.var l in
  let params =
    let$ w = Lwd.get box_w and$ l = Lwd.get box_l in
    Params.Baggi { w; l }
  in
  let inputs =
    [
      ("Box width", Ui.float_input box_w);
      ("Box length", Ui.float_input box_l);
      ( "Box height",
        let$ w = Lwd.get box_w in
        El.txt' (Printf.sprintf "%.0fmm" w) );
      ( "Paper size",
        let$ pw, ph = paper_size box_w box_l in
        El.txt' (Printf.sprintf "%dmm x %dmm" pw ph) );
    ]
  in
  let image ~measure_text:_ = Lwd.pure I.void in
  ( Ui.box_ui title ~inputs ~image
      ~resources:
        [
          ( "Tuto 1 : Les boîtes baggi",
            "Les ludistes origamistes",
            "https://www.youtube.com/watch?v=ZdtQVv-AxR0" );
        ],
    params )
