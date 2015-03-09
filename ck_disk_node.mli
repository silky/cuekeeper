(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** The data that gets stored to disk (e.g. parent UUID), but not data we calculate on loading
 * (e.g. list of children). *)

open Ck_sigs

include DISK_NODE
open Types

val of_string : string -> [action | project | area]
val to_string : [< action | project | area] -> string

val equal : 'a t -> 'a t -> bool

val make_action : name:string -> description:string -> parent:Ck_id.t -> ctime:float ->[> action]
val make_project : name:string -> description:string -> parent:Ck_id.t -> ctime:float ->[> project]
val make_area : name:string -> description:string -> parent:Ck_id.t -> ctime:float ->[> area]

val with_name : generic -> string -> generic
val with_parent : generic -> Ck_id.t -> generic
val with_astate : action_data -> [ `Next | `Waiting | `Future | `Done ] -> action_data
val with_pstate : project_data -> [ `Active | `SomedayMaybe | `Done ] -> project_data
val with_starred : [< project | action] -> bool -> [project | action]

val as_area : project_data -> area_data
val as_project : [< area | action] -> project_data
val as_action : project_data -> action_data
