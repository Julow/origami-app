open Brr
open Brr_lwd
open Vg
open Lwd_infix
open Draw_utils

let title = "Accordion Style Divider"

type t = { x_folds : float list; y_folds : float list; paper_size : int * int }

let th = 0.1

let image t ~measure_text =
  let$ t = t in
  let pw = float (fst t.paper_size) and ph = float (snd t.paper_size) in
  let fw, fh =
    if pw < ph then (50. *. pw /. ph, 50.) else (50., 50. *. ph /. pw)
  in
  let fx, fy = (fw /. -2., fh /. -2.) in
  let x_unit = fw /. pw and y_unit = fh /. ph in
  let rot_text = Float.pi /. -4. in
  let labels_x =
    List.fold_left
      (fun acc fold ->
        let x = fx +. ((pw -. fold) *. x_unit) in
        let rot_line = Float.pi /. -2. in
        acc ++ label_left ~measure_text ~rot_line ~rot_text x ~-.fy (Ui.mm fold))
      I.void t.x_folds
  in
  let labels_y =
    List.fold_left
      (fun acc fold ->
        let y = fy +. ((ph -. fold) *. y_unit) in
        acc ++ label_right ~rot_text ~-.fx y (Ui.mm fold))
      I.void t.y_folds
  in
  feuille ~w:fw ~h:fh () ++ labels_x ++ labels_y |> I.rot (Float.pi /. 4.)

let compute_t box_w box_h compartments =
  let$ w = Lwd.get box_w and$ h = Lwd.get box_h and$ compartments in
  let compartments = Lwd_seq.to_list compartments in
  let n = List.length compartments in
  let total_size = List.fold_left ( +. ) 0. compartments in
  let paper_w = int_of_float ((2. *. h) +. w) in
  let paper_h =
    int_of_float (total_size +. (float (n - 1) *. ((2. *. h) -. (6. *. th))))
  in
  let x_folds = [ h; h +. w ] in
  let y_folds =
    let ( @+ ) length (acc, y) = (y :: acc, y +. length) in
    match List.rev compartments with
    | hd :: tl ->
        fst (List.fold_left (fun acc c -> c @+ h @+ h @+ acc) ([ hd ], hd) tl)
    | [] -> []
  in
  { x_folds; y_folds; paper_size = (paper_w, paper_h) }

let ui () =
  let box_w = Lwd.var 60. in
  let box_h = Lwd.var 30. in
  let compartments = Lwd_table.make () in
  (* Storing variables inside the table to avoid unwanted DOM updates when an
     input changes value. *)
  ignore (Lwd_table.append ~set:(Lwd.var 50.) compartments);
  ignore (Lwd_table.append ~set:(Lwd.var 50.) compartments);
  let add_compartment _ =
    ignore (Lwd_table.append ~set:(Lwd.var 50.) compartments)
  in
  let remove_compartment row _ = Lwd_table.remove row in
  let compartments_sizes =
    Lwd_seq.lift
      (Lwd_table.map_reduce
         (fun _row v -> Lwd_seq.element (Lwd.get v))
         Lwd_seq.monoid compartments)
  in
  let t = compute_t box_w box_h compartments_sizes in
  let compartment_count =
    Lwd_table.map_reduce (fun _ _ -> 1) (0, ( + )) compartments
  in
  let compartment_row row v =
    let remove_button =
      let hidden =
        let$ compartment_count in
        At.if' (compartment_count <= 2) At.hidden
      in
      Ui.button ~at:[ `R hidden ] "×" (remove_compartment row)
    in
    Lwd_seq.element
      (Ui.input_row "Compartment"
         ~extra_cols:[ `R (Elwd.td [ `R remove_button ]) ]
         (Ui.float_input v))
  in
  let comp_add_row =
    let count =
      let$ compartment_count in
      Ui.txtf "%d compartments" compartment_count
    in
    Elwd.tr
      [
        `R (Elwd.td ~at:[ `P (At.int (Jstr.v "colspan") 2) ] [ `R count ]);
        `R (Ui.button "+" add_compartment);
      ]
  in
  let paper_size_txt =
    let$ { paper_size = w, h; _ } = t in
    El.txt' (Printf.sprintf "%dmm x %dmm" w h)
  in
  let input_rows =
    [
      `R (Ui.input_row "Box width" (Ui.float_input box_w));
      `R (Ui.input_row "Box height" (Ui.float_input box_h));
      `R comp_add_row;
      `S
        (Lwd_seq.lift
           (Lwd_table.map_reduce compartment_row Lwd_seq.monoid compartments));
      `R (Ui.input_row "Paper size" paper_size_txt);
    ]
  in
  Ui.box_ui' title ~input_rows ~image:(image t)
    ~resources:
      [
        ( "[EN] Tutorial #5 : Katta « cutter » dividers",
          "Les ludistes origamistes",
          "https://www.youtube.com/watch?v=aRs5pmxcjlw" );
      ]
