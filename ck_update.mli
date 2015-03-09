(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Making changes to the store. *)

open Ck_sigs

module Make(Git : Git_storage_s.S)
           (R : sig
             include REV with type commit = Git.Commit.t
             val make : Git.Commit.t -> t Lwt.t
             val disk_node : [< Node.generic] -> Ck_disk_node.generic
             val action_node : Node.Types.action_data -> Ck_disk_node.Types.action_data
             val project_node : Node.Types.project_data -> Ck_disk_node.Types.project_data
             val area_node : Node.Types.area_data -> Ck_disk_node.Types.area_data
           end) : sig
  type t
  type update_cb = Git.Commit.t -> unit Lwt.t

  open R.Node.Types

  val make : on_update:update_cb Lwt.t -> Git.Branch.t -> t Lwt.t
  (** Manage updates to this branch.
   * Calls [on_update] after the branch has changed (either due to the methods below or because
   * the store has been modified by another process. *)

  val head : t -> Git.Commit.t
  (** The current tip of the branch. *)

  (** Functions for making updates all work in the same way.
   * 1. Make a new branch from the commit that produced the source item.
   * 2. Commit the change to that branch (this should always succeed).
   * 3. Merge the new branch to master.
   * 4. Call the [on_update] function.
   * When they return, on_update has completed for the new revision. *)

  val add : t -> ?uuid:Ck_id.t ->
    parent:[`Toplevel of R.t | `Node of R.Node.generic] ->
    (parent:Ck_id.t -> ctime:float -> Ck_disk_node.generic) ->
    Ck_id.t Lwt.t
  val delete : t -> [< action | project | area] -> unit or_error Lwt.t

  val set_name : t -> [< action | project | area] -> string -> unit Lwt.t
  val set_starred : t -> [< action | project] -> bool -> unit Lwt.t
  val set_action_state : t -> action_data -> [ `Next | `Waiting | `Future | `Done ] -> unit Lwt.t
  val set_project_state : t -> project_data -> [ `Active | `SomedayMaybe | `Done ] -> unit Lwt.t

  val set_a_parent : t -> [area] -> [area] -> unit Lwt.t
  val set_pa_parent : t -> [< project | action] -> [< area | project] -> unit Lwt.t
  val remove_parent : t -> [< area | project | action] -> unit Lwt.t

  val convert_to_area : t -> project_data -> unit or_error Lwt.t
  val convert_to_project : t -> [< action | area] -> unit or_error Lwt.t
  val convert_to_action : t -> project_data -> unit or_error Lwt.t
end
