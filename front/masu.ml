open Brr
open Vg
open Lwd_infix

let title = "Masu"
let paper_width box_w = box_w /. 0.3536

let ui () =
  let box_w = Lwd.var 100. in
  let inputs =
    [
      ("Box width", Ui.float_input box_w);
      ( "Box length",
        let$ w = Lwd.get box_w in
        El.txt' (Printf.sprintf "%.0fmm" w) );
      ( "Box height",
        let$ w = Lwd.get box_w in
        El.txt' (Printf.sprintf "%.0fmm" (w /. 2.)) );
      ( "Paper size",
        let$ w = Lwd.get box_w in
        let p = paper_width w in
        El.txt' (Printf.sprintf "%.0fmm x %.0fmm" p p) );
    ]
  in
  let image ~measure_text:_ = Lwd.pure I.void in
  Ui.box_ui title ~inputs ~image
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
