open Brr
open Brr_lwd

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
