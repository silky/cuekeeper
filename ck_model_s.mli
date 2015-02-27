(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module type MODEL = sig
  type t
  type 'a full_node

  module View : sig
    type t = {
      uuid : Ck_id.t;
      init_node_type : [ area | project | action ]; (* Hack for "signal value undefined yet" *)
      node_type : [ area | project | action | `Deleted ] React.S.t;
      ctime : float;
      name : string React.S.t;
      description : string React.S.t;
      child_views : t ReactiveData.RList.t;
      state : int Slow_set.state React.S.t;
    }
  end

  module TreeNode : sig
    module Item : sig
      type t
      val id : t -> Ck_id.t 
      val node : t -> [area | project | action] Ck_disk_node.t
    end
  end

  module WorkTree : sig
    module Widget : sig
      (** An object visible on the screen. *)

      type t
      val id : t -> Ck_id.t
      val item : t -> TreeNode.Item.t React.S.t
      val children : t -> t ReactiveData.RList.t
      val state : t -> int Slow_set.state React.S.t
    end
  end

  val root : t -> [area] full_node React.S.t
  val is_root : Ck_id.t -> bool

  val all_areas_and_projects : t -> (string * [> area | project] full_node) list

  val uuid : _ full_node -> Ck_id.t

  val add_action : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_project : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t
  val add_area : t -> parent:Ck_id.t -> name:string -> description:string -> Ck_id.t Lwt.t

  val delete : t -> Ck_id.t -> unit Lwt.t

  val set_name : t ->  Ck_id.t -> string -> unit Lwt.t
  val set_details : t -> Ck_id.t -> [< action | project | area] -> unit Lwt.t
  val set_starred : t -> Ck_id.t -> bool -> unit Lwt.t

  val process_tree : t -> View.t
  val work_tree : t -> WorkTree.Widget.t ReactiveData.RList.t
  val details : t -> Ck_id.t -> View.t
  val history : t -> (float * string) list React.S.t
end
