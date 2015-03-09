(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std

type node_details = {
  parent : Ck_id.t;
  name : string;
  description : string;
  ctime : float with default(0.0);
} with sexp

type action_details = {
  astarred : bool with default(false);
  astate : [ `Next | `Waiting | `Future | `Done ]
} with sexp

type project_details = {
  pstarred : bool with default(false);
  pstate : [ `Active | `SomedayMaybe | `Done ]
} with sexp

type generic =
  [ `Action of (action_details * node_details)
  | `Project of (project_details * node_details)
  | `Area of node_details ]
  with sexp

module Types = struct
  type action_data = action_details * node_details
  type project_data = project_details * node_details
  type area_data = node_details

  type action = [`Action of action_data]
  type project = [`Project of project_data]
  type area = [`Area of area_data]
end
open Types

type +'a t = [< generic] as 'a

let details = function
  | `Action (_, d)
  | `Project (_, d)
  | `Area d -> d

let ctime t = (details t).ctime
let name t = (details t).name
let description t = (details t).description
let parent t = (details t).parent

let of_string s = generic_of_sexp (Sexplib.Sexp.of_string s)
let to_string (t : [< action | project | area]) = Sexplib.Sexp.to_string (sexp_of_generic (t :> generic))

let make ~name ~description ~parent ~ctime = {
  name;
  description;
  parent;
  ctime;
}

let map_details fn = function
  | `Action (x, d) -> `Action (x, fn d)
  | `Project (x, d) -> `Project (x, fn d)
  | `Area d -> `Area (fn d)

let with_name node name = node |> map_details (fun d -> {d with name})
let with_parent node parent = node |> map_details (fun d -> {d with parent})
let equal : _ t -> _ t -> bool = (=)

let action_state ({ astate; _ }, _ ) = astate
let project_state ({ pstate; _ }, _ ) = pstate
let starred = function
  | `Project ({ pstarred; _ }, _ ) -> pstarred
  | `Action ({ astarred; _ }, _) -> astarred

let with_astate (a, details) astate = ({a with astate}, details)
let with_pstate (p, details) pstate = ({p with pstate}, details)

let with_starred node s =
  match node with
  | `Action (a, d) -> `Action ({a with astarred = s}, d)
  | `Project (p, d) -> `Project ({p with pstarred = s}, d)

let make_action ~name ~description ~parent ~ctime =
  `Action ({ astate = `Next; astarred = false }, make ~name ~description ~parent ~ctime)

let make_project ~name ~description ~parent ~ctime =
  `Project ({ pstate = `Active; pstarred = false }, make ~name ~description ~parent ~ctime)

let make_area ~name ~description ~parent ~ctime =
  `Area (make ~name ~description ~parent ~ctime)

let is_done = function
  | `Action ({ astate; _}, _) -> astate = `Done
  | `Project ({ pstate; _}, _) -> pstate = `Done

let as_project = function
  | `Action ({ astarred; _}, d) -> ({pstate = `Active; pstarred = astarred}, d)
  | `Area d -> ({pstate = `Active; pstarred = false}, d)

let as_area (_, d) = d

let as_action ({ pstarred; _}, d) = ({astate = `Next; astarred = pstarred}, d)
