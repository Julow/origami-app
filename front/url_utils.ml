open Brr

let set_url_hash hash =
  let uri = Window.location G.window in
  match Uri.with_uri ~fragment:(Jstr.v hash) uri with
  | Ok uri -> Window.History.replace_state ~uri (Window.history G.window)
  | Error _ -> ()

let get_url_hash () = Jstr.to_string (Uri.fragment (Window.location G.window))

(** Add an history entry. *)
let push () = Window.History.push_state (Window.history G.window)

(** Synchronise the state of the application and the hash part of the URL. The
    URL is updated everytime [url_hash] changes. [on_change] is called when the
    URL is changed by the user (either manually or by going back in the
    history). Returns the initial hash. *)
let start ~on_change url_hash =
  let url_hash = Lwd.observe url_hash in
  let on_invalidate _ =
    ignore
      (G.request_animation_frame (fun _ ->
           set_url_hash (Lwd.quick_sample url_hash)))
  in
  set_url_hash (Lwd.quick_sample url_hash);
  Lwd.set_on_invalidate url_hash on_invalidate;
  ignore
    (Ev.listen Window.History.Ev.popstate
       (fun _ -> on_change (get_url_hash ()))
       (Window.as_target G.window));
  get_url_hash ()
