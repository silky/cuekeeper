(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_utils
open Ck_sigs

module Node = Ck_node
module M = Ck_node.M

module Raw(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module Top = Graph.Topological.Make(I.History)

  type t = {
    store : string -> I.t;
    commit : I.head;
    root : 'a. ([> area] as 'a) Node.t;
    index : (Ck_id.t, Node.generic) Hashtbl.t;
    history : (float * string) list;
  }

  let eq a b =
    a.commit = b.commit

  let rec walk fn node =
    fn node;
    node.Node.child_nodes |> M.iter (fun _k v -> walk fn v)

  let get_current store =
    I.head (store "Get latest commit") >>= function
    | Some commit -> return commit
    | None ->
        I.update (store "Init") ["ck-version"] "0.1" >>= fun () ->
        I.head_exn (store "Get initial commit")

  let make store =
    get_current store >>= fun commit ->
    (* TODO: do all reads using this commit *)
    let disk_nodes = Hashtbl.create 100 in
    let children = Hashtbl.create 100 in
    Hashtbl.add disk_nodes Ck_id.root Ck_disk_node.root;
    I.list (store "Find db nodes") ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          let uuid = Ck_id.of_string uuid in
          assert (uuid <> Ck_id.root);
          I.read_exn (store "Load db node") key >|= fun s ->
          let node = Ck_disk_node.of_string s in
          Hashtbl.add disk_nodes uuid node;
          let parent = Ck_disk_node.parent node in
          let old_children =
            try Hashtbl.find children parent
            with Not_found -> [] in
          Hashtbl.replace children parent (uuid :: old_children);
      | _ -> assert false
    ) >>= fun () ->
    children |> Hashtbl.iter (fun parent children ->
      if not (Hashtbl.mem disk_nodes parent) then (
        error "Parent UUID '%a' of child nodes %s missing!" Ck_id.fmt parent (String.concat ", " (List.map Ck_id.to_string children))
      )
    );

    (* todo: reject cycles *)
    let rec make_node uuid =
      let disk_node = Hashtbl.find disk_nodes uuid in
      Node.make ~uuid ~disk_node ~child_nodes:(make_child_nodes uuid)
    and make_child_nodes uuid =
      begin try Hashtbl.find children uuid with Not_found -> [] end
      |> List.map make_node
      |> List.fold_left (fun set node ->
          M.add (Node.key node) node set
        ) M.empty in

    let root = Node.make_root ~child_nodes:(make_child_nodes Ck_id.root) in
    let index = Hashtbl.create 100 in
    root |> walk (fun node -> Hashtbl.add index (Node.uuid node) node);
    I.history ~depth:10 (store "Read history") >>= fun history ->
    let h = ref [] in
    history |> Top.iter (fun head ->
      h := head :: !h
    );
    !h |> Lwt_list.map_s (fun hash ->
      I.task_of_head (store "Read commit") hash >|= fun task ->
      let summary =
        match Irmin.Task.messages task with
        | [] -> "(no commit message)"
        | x::_ -> x in
      let date = Irmin.Task.date task |> Int64.to_float in
      (date, summary)
    ) >|= fun history ->
    { store; commit; root; index; history}

  let get t uuid =
    try Some (Hashtbl.find t.index uuid)
    with Not_found -> None

  let get_exn t uuid =
    try Hashtbl.find t.index uuid
    with Not_found -> error "UUID '%a' not found in database!" Ck_id.fmt uuid

  (* Note: in theory, the result might not match the input type, if the
   * merge changes it for some reason. In practice, this shouldn't happen. *)
  let create t (node:_ Ck_disk_node.t) =
    let uuid = Ck_id.mint () in
    assert (not (Hashtbl.mem t.index uuid));
    let parent = Ck_disk_node.parent node in
    if not (Hashtbl.mem t.index parent) then
      error "Parent '%a' does not exist!" Ck_id.fmt parent;
    let s = Ck_disk_node.to_string node in
    let msg = Printf.sprintf "Create '%s'" (Ck_disk_node.name node) in
    I.update (t.store msg) ["db"; Ck_id.to_string uuid] s >>= fun () ->
    make t.store >|= fun t_new ->
    (Hashtbl.find t_new.index uuid, t_new)

  let update t ~msg node =
    let node = (node :> Node.generic) in
    assert (Hashtbl.mem t.index (Node.uuid node));
    if not (Hashtbl.mem t.index (Node.parent node)) then
      error "Parent '%a' does not exist!" Ck_id.fmt (Node.parent node);
      let s = Ck_disk_node.to_string node.Node.disk_node in
    I.update (t.store msg) ["db"; Ck_id.to_string node.Node.uuid] s >>= fun () ->
    make t.store

  let delete t uuid =
    assert (uuid <> Ck_id.root);
    let node = get_exn t uuid in
    let msg = Printf.sprintf "Delete '%s'" (Node.name node) in
    I.remove (t.store msg) ["db"; Ck_id.to_string uuid] >>= fun () ->
    make t.store
end

module Make(Clock : Ck_clock.S)(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Raw(I)

  module TreeNode = struct
    module Id_map = Ck_id.M
    module Child_map = M
    module Sort_key = Node.SortKey

    module Item = struct 
      type t = Node.generic   (* Ignore children, though *)

      let equal a b =
        Node.uuid a = Node.uuid b &&
        Ck_disk_node.equal a.Node.disk_node b.Node.disk_node

      let compare a b =
        let open Node in
        match String.compare (name a) (name b) with
        | 0 -> compare (uuid a) (uuid b)
        | r -> r

      let show = Node.name
      let id = Node.uuid
      let node n = n.Node.disk_node
    end

    type t = {
      item : Node.generic;
      children : t M.t;
    }

    let item t = t.item
    let children t = t.children

    let leaf_of_node n = {
      item = n;
      children = M.empty;
    }

    let id t = Item.id t.item

    let rec equal a b =
      Item.equal a.item b.item &&
      Child_map.equal equal a.children b.children

    type move_data = int
  end
  module WorkTree = Reactive_tree.Make(Clock)(TreeNode)

  type t = {
    current : R.t React.S.t;
    set_current : R.t -> unit;
    work_tree : WorkTree.t;
  }

  type 'a full_node = 'a Node.t

  module View = struct
    type t = {
      uuid : Ck_id.t;
      init_node_type : [ area | project | action ];
      node_type : [ area | project | action | `Deleted ] React.S.t;
      ctime : float;
      name : string React.S.t;
      description : string React.S.t;
      child_views : t ReactiveData.RList.t;
      state : int Slow_set.state React.S.t;
    }

    let equal a b =
      a.uuid = b.uuid &&
      a.init_node_type = b.init_node_type &&
      a.ctime = b.ctime &&
      a.state == b.state
      (* We ignore the signals, since any view with the same
       * uuid with have the same signals values. *)
  end

  module Slow = Slow_set.Make(Clock)(Node.SortKey)(M)
  module NodeList = Delta_RList.Make(Node.SortKey)(View)(M)

  let assume_changed _ _ = false

  let root t = t.current |> React.S.map ~eq:assume_changed (fun r -> r.R.root)
  let is_root = (=) Ck_id.root

  let all_areas_and_projects t =
    let results = ref [] in
    let rec scan prefix x =
      let full_path = prefix ^ "/" ^ Node.name x in
      results := (full_path, x) :: !results;
      Node.child_nodes x |> M.iter (fun _k child ->
        match child with
        | {Node.disk_node = {Ck_disk_node.details = `Area | `Project _; _}; _} as x -> scan full_path x
        | _ -> ()
      ) in
    scan "" (root t |> React.S.value);
    List.rev !results

  let uuid = Node.uuid

  let add details t ~parent ~name ~description =
    let disk_node =
      Ck_disk_node.make ~name ~description ~parent ~ctime:(Unix.gettimeofday ()) ~details in
    let r = React.S.value t.current in
    R.create r disk_node >|= fun (node, r_new) ->
    t.set_current r_new;
    node.Node.uuid

  let add_action = add (`Action {Ck_sigs.astate = `Next; astarred = false})
  let add_project = add (`Project {Ck_sigs.pstate = `Active; pstarred = false})
  let add_area = add `Area

  let delete t uuid =
    let r = React.S.value t.current in
    R.delete r uuid >|= t.set_current

  let set_name t uuid name =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    let new_node = Node.with_name node name in
    let msg = Printf.sprintf "Rename '%s' to '%s'" (Node.name node) (Node.name new_node) in
    R.update r ~msg new_node >|= t.set_current

  let set_details t uuid new_details =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    let new_node = Node.with_details node new_details in
    let msg = Printf.sprintf "Change state of '%s'" (Node.name node) in
    R.update r ~msg new_node >|= t.set_current

  let set_starred t uuid s =
    let r = React.S.value t.current in
    let node = R.get_exn r uuid in
    let new_node =
      match node with
      | {Node.disk_node = {Ck_disk_node.details = `Action d; _}; _} as n -> Node.with_details n (`Action {d with astarred = s})
      | {Node.disk_node = {Ck_disk_node.details = `Project d; _}; _} as n -> Node.with_details n (`Project {d with pstarred = s})
      | _ -> error "Not a project or action node: '%s'" (Node.name node) in
    let action = if s then "Add" else "Remove" in
    let msg = Printf.sprintf "%s star for '%s'" action (Node.name node) in
    R.update r ~msg new_node >|= t.set_current

  let node_type {Node.disk_node = {Ck_disk_node.details; _}; _} = details
  let opt_node_type = function
    | None -> `Deleted
    | Some x -> (node_type x :> [action | project | area | `Deleted])
  let opt_node_name = function
    | None -> "(deleted)"
    | Some x -> Node.name x
  let opt_node_description = function
    | None -> "(deleted)"
    | Some x -> Node.description x
  let opt_child_nodes = function
    | None -> M.empty
    | Some x -> Node.child_nodes x

  type child_filter = {
(*     pred : Node.generic -> bool;        (* Whether to include a child *) *)
    render : (Node.generic, int) Slow_set.item -> View.t;    (* How to render it *)
  }

  let opt_node_eq a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> Node.equal a b
    | _ -> false

  let render_node ?child_filter t (node, state) =
    let live_node = t.current |> React.S.map ~eq:opt_node_eq (fun r -> R.get r node.Node.uuid) in
    let child_views =
      match child_filter with
      | None -> ReactiveData.RList.empty
      | Some filter -> live_node
          |> React.S.map ~eq:(M.equal Node.equal) opt_child_nodes
          |> Slow.make ~eq:Node.equal ~init:node.Node.child_nodes ~delay:1.0
          |> React.S.map ~eq:(M.equal View.equal) (M.map filter.render)
          |> NodeList.make in
    { View.
      uuid = Node.uuid node;
      ctime = Node.ctime node;
      init_node_type = Node.details node;
      node_type = live_node |> React.S.map opt_node_type;
      name = live_node |> React.S.map opt_node_name;
      description = live_node |> React.S.map opt_node_description;
      child_views;
      state;
    }

  let render_slow_node ?child_filter t item =
    let node = Slow_set.data item in
    let state = Slow_set.state item in
    render_node ?child_filter t (node, state)

  let process_tree t =
    let root_node = R.get_exn (React.S.value t.current) Ck_id.root in
    let rec child_filter = {
      render = (fun n -> render_slow_node ~child_filter t n);
    } in
    render_node t ~child_filter (root_node, React.S.const `Current)

  let is_next_action _k = function
    | {Node.disk_node = {Ck_disk_node.details = `Action {astate = `Next; _}; _}; _} -> true
    | _ -> false

  let collect_next_actions r =
    let results = ref TreeNode.Child_map.empty in
    let rec scan = function
      | {Node.disk_node = {Ck_disk_node.details = `Area | `Project _; _}; _} as parent ->
          let actions = Node.child_nodes parent |> M.filter is_next_action in
          if not (M.is_empty actions) then (
            let tree_node = { TreeNode.
              item = parent;
              children = actions |> M.map TreeNode.leaf_of_node
            } in
            results := !results |> M.add (Node.key parent) tree_node;
          );
          Node.child_nodes parent |> M.iter (fun _k v -> scan v)
      | {Node.disk_node = {Ck_disk_node.details = `Action _; _}; _} -> ()
    in
    scan r.R.root;
    !results

  let work_tree t = WorkTree.widgets t.work_tree
(*
    |> Slow.make ~eq:Node.eq ~delay:1.0
    |> React.S.map ~eq:(M.equal View.eq) (M.map (render_slow_node ~child_filter t))
    |> NodeList.make
*)

  let details t uuid =
    let initial_node = R.get_exn (React.S.value t.current) uuid in
    let child_filter = {
      render = render_slow_node t;
    } in
    render_node t ~child_filter (initial_node, React.S.const `Current)

  let history t =
    t.current >|~= fun r -> r.R.history

  let make store =
    R.make store >|= fun r ->
    let current, set_current = React.S.create ~eq:R.eq r in
    let work_tree = current
      |> React.S.map ~eq:(M.equal TreeNode.equal) collect_next_actions
      |> WorkTree.make in
    { current; set_current; work_tree }
end
