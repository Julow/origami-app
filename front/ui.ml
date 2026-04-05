open Brr
open Brr_lwd
open Vg
open Gg

let ( ++ ) a b = I.blend b a

let font =
  { Font.name = "sans-serif"; slant = `Normal; weight = `W400; size = 2.5 }

(** Draw text centered around [0] on the [x] axis. *)
let text_centered ~measure_text font color text =
  I.cut_glyphs ~text font [] (I.const color)
  |> I.move (V2.v (~-.(measure_text font text) /. 2.) 0.)

(** Draw a rect centered on the given position with the given size. *)
let rect_mid pos size color =
  let p = P.empty |> P.rect (Box2.v_mid pos size) in
  I.cut p (I.const color)

let feuille color = rect_mid P2.o (Size2.v 50. 50.) color

let diag angle w =
  (* Drawing a rectangle instead of a line with a outline because the canvas
     backend doesn't support cutting an outline on an arbitrary image. *)
  let rect = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 100. w)) in
  let rect = P.tr (M3.rot2 angle) rect in
  I.cut rect (feuille Color.black)

let float_input var =
  let on_input ev =
    let target = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
    let value = El.prop El.Prop.value target in
    Lwd.set var (Jstr.to_float value)
  in
  Elwd.input
    ~at:
      [
        `P (At.type' (Jstr.v "number"));
        `P (At.value (Jstr.of_float (Lwd.peek var)));
      ]
    ~ev:[ `P (Elwd.handler Ev.input on_input) ]
    ()
