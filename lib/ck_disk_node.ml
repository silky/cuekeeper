(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Conv
open Ck_utils

type node_details = {
  parent : Ck_id.t;
  name : string;
  description : string;
  ctime : float;
  contact : Ck_id.t sexp_option;
  conflicts : string sexp_list;
} with sexp

type astate =
  [ `Next
  | `Waiting
  | `Waiting_for_contact
  | `Waiting_until of Ck_time.user_date
  | `Future
  | `Done ] with sexp

type action_details = {
  astarred : bool with default(false);
  astate : astate;
  context : Ck_id.t sexp_option;
  repeat: Ck_time.repeat sexp_option;
} with sexp

type project_details = {
  pstarred : bool with default(false);
  pstate : [ `Active | `SomedayMaybe | `Done ]
} with sexp

type apa =
  [ `Action of (action_details * node_details)
  | `Project of (project_details * node_details)
  | `Area of node_details ]
  with sexp

type generic =
  [ apa
  | `Contact of node_details
  | `Context of node_details ]

module Types = struct
  type action_node = action_details * node_details
  type project_node = project_details * node_details
  type area_node = node_details
  type contact_node = node_details
  type context_node = node_details

  type action = [`Action of action_node]
  type project = [`Project of project_node]
  type area = [`Area of area_node]
  type contact = [`Contact of contact_node]
  type context = [`Context of context_node]
end

let details = function
  | `Action (_, d)
  | `Project (_, d)
  | `Area d -> d
  | `Contact d -> d
  | `Context d -> d

let ctime t = (details t).ctime
let name t = (details t).name
let description t = (details t).description
let conflicts t = (details t).conflicts
let contact t = (details t).contact
let parent t = (details t).parent

let of_string s = apa_of_sexp (Sexplib.Sexp.of_string s)
let to_string t = Sexplib.Sexp.to_string (sexp_of_apa (t :> apa))

let contact_of_string s = `Contact (node_details_of_sexp (Sexplib.Sexp.of_string s))
let contact_to_string (`Contact t) = Sexplib.Sexp.to_string (sexp_of_node_details t)

let context_of_string s = `Context (node_details_of_sexp (Sexplib.Sexp.of_string s))
let context_to_string (`Context t) = Sexplib.Sexp.to_string (sexp_of_node_details t)

let make ~name ~description ~parent ~ctime ~contact = {
  name;
  description;
  parent;
  ctime;
  contact;
  conflicts = [];
}

let map_apa fn = function
  | `Action (x, d) -> `Action (x, fn d)
  | `Project (x, d) -> `Project (x, fn d)
  | `Area d -> `Area (fn d)

let map_details fn = function
  | `Action _ | `Project _ | `Area _ as node -> map_apa fn node
  | `Contact d -> `Contact (fn d)
  | `Context d -> `Context (fn d)

let with_name node name = node |> map_details (fun d -> {d with name})
let with_description node description = node |> map_details (fun d -> {d with description})
let with_parent node parent = node |> map_apa (fun d -> {d with parent})
let with_contact node contact = node |> map_apa (fun d -> {d with contact})
let equal = (=)

let context (`Action (action_details, _)) = action_details.context
let action_repeat (`Action ({ repeat; _ }, _)) = repeat
let action_state (`Action ({ astate; _ }, _)) = astate
let project_state (`Project ({ pstate; _ }, _)) = pstate
let starred = function
  | `Project ({ pstarred; _ }, _ ) -> pstarred
  | `Action ({ astarred; _ }, _) -> astarred

let with_repeat (`Action (a, details)) repeat = `Action ({a with repeat}, details)
let with_astate (`Action (a, details)) astate = `Action ({a with astate}, details)
let with_pstate (`Project (p, details)) pstate = `Project ({p with pstate}, details)

let with_starred node s =
  match node with
  | `Action (a, d) -> `Action ({a with astarred = s}, d)
  | `Project (p, d) -> `Project ({p with pstarred = s}, d)

let with_context (`Action (a, details)) context = `Action ({a with context}, details)

let make_action ~state ?context ?contact ~name ~description ~parent ~ctime () =
  `Action ({ astate = state; astarred = false; context; repeat = None }, make ~name ~description ~parent ~ctime ~contact)

let make_project ~state ~name ~description ~parent ~ctime () =
  `Project ({ pstate = state; pstarred = false }, make ~name ~description ~parent ~ctime ~contact:None)

let make_area ~name ~description ~parent ~ctime () =
  `Area (make ~name ~description ~parent ~ctime ~contact:None)

let make_contact ~name ~description ~ctime () =
  `Contact (make ~name ~description ~parent:Ck_id.root ~ctime ~contact:None)

let make_context ~name ~description ~ctime () =
  `Context (make ~name ~description ~parent:Ck_id.root ~ctime ~contact:None)

let is_done = function
  | `Action ({ astate; _}, _) -> astate = `Done
  | `Project ({ pstate; _}, _) -> pstate = `Done

let as_project = function
  | `Action ({ astarred; _}, d) -> `Project ({pstate = `Active; pstarred = astarred}, d)
  | `Project _ as p -> p
  | `Area d -> `Project ({pstate = `Active; pstarred = false}, d)

let as_area (`Project (_, d)) = `Area d

let as_action = function
  | `Project ({ pstarred; _}, d) -> `Action ({astate = `Next; astarred = pstarred; context = None; repeat = None}, d)
  | `Action _ as a -> a
  | `Area d -> `Action ({astate = `Next; astarred = false; context = None; repeat = None}, d)

let merge_detail ~log ~fmt ~base ~theirs ours =
  if base = theirs then ours
  else if base = ours then theirs
  else if theirs = ours then theirs
  else (
    Printf.sprintf "Discarded change %s -> %s\n" (fmt base) (fmt theirs) |> log;
    ours
  )

(* Used for the (unlikely) case of a merge with no common ancestor *)
let default_base = make ~name:"" ~description:"" ~parent:Ck_id.root ~ctime:0.0 ~contact:None

let opt_uuid = function
  | None -> "(none)"
  | Some uuid -> Ck_id.to_string uuid
let fmt_repeat = function
  | None -> "never"
  | Some repeat -> Ck_time.string_of_repeat repeat
let str x = x
let star = function
  | false -> "unstarred"
  | true -> "starred"
let fmt_pstate = function
  | `Active -> "active"
  | `Done -> "done"
  | `SomedayMaybe -> "someday/maybe"
let fmt_astate = function
  | `Next -> "next"
  | `Waiting -> "waiting"
  | `Waiting_for_contact -> "waiting for contact"
  | `Waiting_until date -> Printf.sprintf "waiting until %s" (Ck_time.string_of_user_date date)
  | `Future -> "future"
  | `Done -> "done"

let merge_details ~log ~base ~theirs ours =
  let {parent; name; description; ctime; contact; conflicts} = ours in
  let parent      = merge_detail ~log ~fmt:Ck_id.to_string ~base:base.parent ~theirs:theirs.parent parent in
  let name        = merge_detail ~log ~fmt:str ~base:base.name ~theirs:theirs.name name in
  let description = merge_detail ~log ~fmt:str ~base:base.description ~theirs:theirs.description description in
  let ctime       = min (min base.ctime theirs.ctime) ctime in
  let contact     = merge_detail ~log ~fmt:opt_uuid ~base:base.contact ~theirs:theirs.contact contact in
  let conflicts   = conflicts @ theirs.conflicts in    (* TODO: eliminate duplicates *)
  {parent; name; description; ctime; contact; conflicts}

let merge_project ~log ~base ~theirs ours =
  let `Project (base_prj, base_details) = base in
  let (their_prj, their_details) = theirs in
  let ({pstarred; pstate}, our_details) = ours in
  let prj = {
    pstarred = merge_detail ~log ~fmt:star ~base:base_prj.pstarred ~theirs:their_prj.pstarred pstarred;
    pstate = merge_detail ~log ~fmt:fmt_pstate ~base:base_prj.pstate ~theirs:their_prj.pstate pstate;
  } in
  `Project (prj, merge_details ~log ~base:base_details ~theirs:their_details our_details)

let merge_action ~log ~base ~theirs ours =
  let `Action (base_act, base_details) = base in
  let (their_act, their_details) = theirs in
  let ({astarred; astate; context; repeat}, our_details) = ours in
  let act = {
    astarred = merge_detail ~log ~fmt:star ~base:base_act.astarred ~theirs:their_act.astarred astarred;
    astate = merge_detail ~log ~fmt:fmt_astate ~base:base_act.astate ~theirs:their_act.astate astate;
    context = merge_detail ~log ~fmt:opt_uuid ~base:base_act.context ~theirs:their_act.context context;
    repeat = merge_detail ~log ~fmt:fmt_repeat ~base:base_act.repeat ~theirs:their_act.repeat repeat;
  } in
  `Action (act, merge_details ~log ~base:base_details ~theirs:their_details our_details)

let merge ?base ~theirs ours =
  let base = (base :> apa option) |> default (`Area default_base) in
  let theirs = (theirs :> apa) in
  let ours = (ours :> apa) in
  if base = theirs then ours
  else if base = ours then theirs
  else (
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged =
      match theirs, ours with
      | `Area theirs, `Area ours -> `Area (merge_details ~log ~base:(details base) ~theirs ours)
      | `Project theirs, `Project ours -> merge_project ~log ~base:(as_project base) ~theirs ours
      | `Action theirs, `Action ours -> merge_action ~log ~base:(as_action base) ~theirs ours
      | theirs, ours ->
          log "Type mismatch: converting to project";
          let `Project theirs = as_project theirs in
          let `Project ours = as_project ours in
          merge_project ~log ~base:(as_project base) ~theirs ours
    in
    merged |> map_apa (fun d -> {d with conflicts = d.conflicts @ !conflicts})
  )

let merge_context ?base ~theirs ours =
  let base = base |> default (`Context default_base) in
  if base = theirs then ours
  else if base = ours then theirs
  else (
    let `Context base = base in
    let `Context theirs = theirs in
    let `Context ours = ours in
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged = merge_details ~log ~base ~theirs ours in
    `Context {merged with conflicts = merged.conflicts @ !conflicts}
  )

let merge_contact ?base ~theirs ours =
  let base = base |> default (`Contact default_base) in
  if base = theirs then ours
  else if base = ours then theirs
  else (
    let `Contact base = base in
    let `Contact theirs = theirs in
    let `Contact ours = ours in
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged = merge_details ~log ~base ~theirs ours in
    `Contact {merged with conflicts = merged.conflicts @ !conflicts}
  )
