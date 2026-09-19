open Brr
open Vg
open Lwd_infix

let title = "Masu"

let paper_width box_w =
  let w = int_of_float (Float.round (box_w /. 0.3536)) in
  (w, w)

let ui { Params.Masu.w } =
  let box_w = Lwd.var w in
  let params =
    let$ w = Lwd.get box_w in
    Params.Masu { w }
  in
  let inputs =
    [
      ([%i18n box_width], Ui.float_input box_w);
      ( [%i18n box_length],
        let$ w = Lwd.get box_w in
        El.txt' (Ui.mm w) );
      ( [%i18n box_height],
        let$ w = Lwd.get box_w in
        El.txt' (Ui.mm (w /. 2.)) );
    ]
  in
  let input_rows =
    [ `R (Ui.paper_size (Lwd.map ~f:paper_width (Lwd.get box_w))) ]
  in
  let image ~measure_text:_ = Lwd.pure I.void in
  let ui =
    Ui.box_ui title ~inputs ~input_rows ~image
      ~resources:
        [
          ( "Tuto 2 : Les boîtes Masu",
            "Les ludistes origamistes",
            "https://www.youtube.com/watch?v=V2q5CyjfEKs" );
          ( "Tutorial 3: Dividers +",
            "Les ludistes origamistes",
            "https://www.youtube.com/watch?v=8r0MMfT0b2I" );
          ( "Tuto 4 : Les diviseurs en X",
            "Les ludistes origamistes",
            "https://www.youtube.com/watch?v=OGsR5NbUCjs" );
        ]
  in
  (ui, params)
