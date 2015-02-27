(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Converts a signal of a tree to a tree of signals.
 * In particular, it gives a persistent identity to nodes, making animations possible.
 *)

open Ck_sigs

module Make (C : Ck_clock.S) (D : DATA) : sig
  type t
  (** Don't let this get GC'd, or updates will stop! *)

  module Widget : sig
    (** An object visible on the screen. *)

    type t
    val id : t -> Ck_id.t
    val item : t -> D.Item.t React.S.t
    val children : t -> t ReactiveData.RList.t
    val state : t -> D.move_data Slow_set.state React.S.t
  end

  val make : D.t D.Child_map.t React.S.t -> t
  val widgets : t -> Widget.t ReactiveData.RList.t
end
