type t

val create : Brr_canvas.Canvas.t -> t
(** Setup [Vgr_htmlc] for a canvas with a size specified with its [width] and
    [height] attributes. This modifies the canvas' properties. *)

val render : t -> Vg.image -> unit
(** Render an image onto the canvas. The viewport is centered on coordinate
    [0, 0] with a height of [100m] and a width that depends on the canvas aspect
    ratio. *)

val measure_text : t -> Vg.Font.t -> string -> float
(** Returns the text width rendered with the given font in [mm]. *)
