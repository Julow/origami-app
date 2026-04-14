open Brr

let set_fragment fragment =
  let uri = Window.location G.window in
  match Uri.with_uri ~fragment:(Jstr.v fragment) uri with
  | Ok uri -> Window.History.replace_state ~uri (Window.history G.window)
  | Error _ -> ()

let get_fragment () = Jstr.to_string (Uri.fragment (Window.location G.window))

(** Add an history entry. *)
let push () = Window.History.push_state (Window.history G.window)

(** Synchronise the state of the application and the URL fragment. The URL is
    updated everytime [fragment] changes. [on_change] is called when the URL is
    changed by the user (either manually or by going back in the history).
    Returns the initial fragment. *)
let start ~on_change fragment =
  let fragment = Lwd.observe fragment in
  let on_invalidate _ =
    ignore
      (G.request_animation_frame (fun _ ->
           set_fragment (Lwd.quick_sample fragment)))
  in
  (* Without this, [on_invalidate] is never called. *)
  ignore (Lwd.quick_sample fragment);
  Lwd.set_on_invalidate fragment on_invalidate;
  ignore
    (Ev.listen Window.History.Ev.popstate
       (fun _ -> on_change (get_fragment ()))
       (Window.as_target G.window));
  get_fragment ()
