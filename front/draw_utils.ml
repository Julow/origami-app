open Vg
open Gg

let ( ++ ) a b = I.blend b a
let font_size = 2.5

let font =
  {
    Font.name = "sans-serif";
    slant = `Normal;
    weight = `W400;
    size = font_size;
  }

(** Draw text centered around [0] on the [x] axis. *)
let text_centered ~measure_text ?(font = font) ?(color = Color.black) text =
  I.cut_glyphs ~text font [] (I.const color)
  |> I.move (V2.v (~-.(measure_text font text) /. 2.) 0.)

(** Draw a rect centered on the given position with the given size. *)
let rect_mid pos size color =
  let p = P.empty |> P.rect (Box2.v_mid pos size) in
  I.cut p (I.const color)

(** [aspect_ratio] is the paper width divided by the paper height. *)
let feuille ?(w = 50.) ?(h = 50.) ?(color = Color.v_srgb 0.314 0.784 0.471) () =
  rect_mid P2.o (Size2.v w h) color

let diag angle w =
  (* Drawing a rectangle instead of a line with a outline because the canvas
     backend doesn't support cutting an outline on an arbitrary image. *)
  let rect = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 100. w)) in
  let rect = P.tr (M3.rot2 angle) rect in
  I.cut rect (feuille ~color:Color.black ())

(** A text on the right of an small axis line. The line can be rotated and is
    horizontal by default. *)
let label_right ?(font = font) ?(color = Color.black) ?(rot_line = 0.)
    ?(rot_text = 0.) x y text =
  (I.cut_glyphs ~text font [] (I.const color)
  |> I.rot rot_text
  |> I.move (V2.v (x +. 2.) (y -. (font.Font.size /. 3.))))
  ++ (rect_mid (P2.v 0. 0.) (Size2.v 2. 0.3) color
     |> I.rot rot_line
     |> I.move (V2.v x y))

(** Like [label_right] but the text is on the left. *)
let label_left ~measure_text ?(font = font) ?(color = Color.black)
    ?(rot_line = 0.) ?(rot_text = 0.) x y text =
  let text_w = measure_text font text in
  (I.cut_glyphs ~text font [] (I.const color)
   (* [rot_text] rotate around the bottom right corner of the text. *)
  |> I.move (V2.v (~-.2. -. text_w) (font.Font.size /. 3.))
  |> I.rot rot_text
  |> I.move (V2.v x y))
  ++ (rect_mid (P2.v 0. 0.) (Size2.v 2. 0.3) color
     |> I.rot rot_line
     |> I.move (V2.v x y))
