(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_utils

type stop = unit -> unit
type 'a or_error = [ `Ok of 'a | `Error of string ]

module type DISK_NODE = sig
  module Types : sig
    type action_data
    type project_data
    type area_data

    type action = [`Action of action_data]
    type project = [`Project of project_data]
    type area = [`Area of area_data]
  end
  open Types

  type generic = [ area | project | action ]
  type +'a t = [< generic] as 'a

  val parent : [< generic] -> Ck_id.t
  val name : [< generic] -> string
  val description : [< generic] -> string
  val ctime : [< generic] -> float
  val starred : [< project | action] -> bool
  val action_state : action_data -> [ `Next | `Waiting | `Future | `Done ]
  val project_state : project_data -> [ `Active | `SomedayMaybe | `Done ]
  val is_done : [< project | action] -> bool
end

module type EQ = sig
  type t
  val equal : t -> t -> bool
end

module type TREE_MODEL = sig
  module Sort_key : Slow_set.SORT_KEY

  module Item : sig
    (** The data part of a node (excluding the child nodes).
     * This is passed through. *)
    type generic
    val equal : generic -> generic -> bool
    val show : generic -> string
  end

  module Child_map : Map.S with type key = Sort_key.t
  (** Ordered list of child nodes. *)

  type t
  type group_id
  val group_label : group_id -> string
  val item : t -> [ `Item of Ck_id.t * Item.generic | `Group of group_id ]
  val children : t -> t Child_map.t
end

module type GUI_DATA = sig
  type t
  (** For extra data the GUI wants to attach to tree nodes. *)
end

module type REV = sig
  type t
  type rev = t

  module Node : sig
    include DISK_NODE
    open Types

    val rev : [< generic] -> rev

    val uuid : [< area | project | action] t -> Ck_id.t
    val child_nodes : 'a t  -> generic M.t

    val key : _ t -> Sort_key.t
    (** A key for sorting by name. *)

    val equal : generic -> generic -> bool
    (** Note that the rev field is ignored, so nodes from different commits can
     * be equal. *)

    val equal_excl_children : generic -> generic -> bool
  end
  open Node.Types

  type commit

  val equal : t -> t -> bool

  val roots : t -> Node.generic M.t
  val history : t -> Git_storage_s.log_entry list   (* XXX: only recent entries *)
  val commit : t -> commit

  val get : t -> Ck_id.t -> Node.generic option
  val parent : t -> [< area | project | action] Node.t -> Node.generic option
end
