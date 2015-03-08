(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module Make (C : Ck_clock.S) (Git : GIT_STORAGE) (G : sig type t end) : sig
  include Ck_model_s.MODEL with
    type gui_data = G.t

  val make : Git.Repository.t -> t Lwt.t
end
