(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

(** A Git-like storage abstraction over Irmin. *)

open Ck_sigs

module Make (I : Irmin.BASIC with type key = string list and type value = string) : sig
  include GIT_STORAGE

  val make : Irmin.config -> (string -> Irmin.task) -> Repository.t
end
