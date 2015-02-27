(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module Make (C : Ck_clock.S) (D : DATA) = struct
  module Slow = Slow_set.Make(C)(D.Sort_key)(D.Child_map)

  module W = struct
    type t = {
      id : D.Id_map.key;
      item : D.Item.t React.S.t;
      set_item : D.Item.t -> unit;
      children : (t, D.move_data) Slow_set.item ReactiveData.RList.t;
      set_child_widgets : ?step:React.step -> t D.Child_map.t -> unit;
    }

    let id t = t.id
    let item t = t.item
    let children t = t.children

    let equal a b = a.id = b.id
    (* Delta wants to know when to send updates. In fact, widgets never update,
     * so we could just return true here. *)
  end

  module Widget = struct
    type t = (W.t, D.move_data) Slow_set.item

    let id t = W.id (Slow_set.data t)
    let item t = W.item (Slow_set.data t)
    let children t = W.children (Slow_set.data t)
    let state = Slow_set.state

    let equal a b =
      W.equal (Slow_set.data a) (Slow_set.data b)
  end

  module Delta = Delta_RList.Make(D.Sort_key)(Widget)(D.Child_map)

  let rec make_widget ~widgets node : W.t =
    (* Printf.printf "make_widget(%s)\n" (D.Item.show (D.item node)); *)
    let children, set_child_widgets = make_widgets ~widgets (D.children node) in
    let item, set_item = React.S.create ~eq:D.Item.equal (D.item node) in
    let id = D.id node in
    let widget = { W.
      id;
      item;
      set_item;
      children;
      set_child_widgets;
    } in
    (* todo: check for duplicates? *)
    widgets := !widgets |> D.Id_map.add id widget;
    widget
  and make_widgets ~widgets nodes =
    let init_children = nodes |> D.Child_map.map (make_widget ~widgets) in
    let child_widgets, set_child_widgets =
      React.S.create ~eq:(D.Child_map.equal W.equal) init_children in
    let children =
      let slow_children =
        child_widgets
        |> Slow.make
            ~delay:1.0
            ~init:init_children
            ~eq:W.equal in
      Delta.make slow_children in
    (children, set_child_widgets)

  type t = {
    widgets : W.t D.Id_map.t ref;
    root_widgets : Widget.t ReactiveData.RList.t;
    mutable keep_me : unit React.S.t option;
  }

  let add_widget t node =
    (* TODO: children might already exist *)
    make_widget ~widgets:t.widgets node

  let rec update t ~old_widgets root_nodes =
    root_nodes |> D.Child_map.map (fun node ->
      let id = D.id node in
      try
        let existing = D.Id_map.find id old_widgets in
        D.children node |> update t ~old_widgets |> existing.W.set_child_widgets;
        t.widgets := !(t.widgets) |> D.Id_map.add id existing;
        existing
      with Not_found -> add_widget t node
    )

  let make input_signal =
    let widgets = ref D.Id_map.empty in
    let root_widgets, set_root_widgets = React.S.value input_signal |> make_widgets ~widgets in
    let t = {
      widgets;
      root_widgets;
      keep_me = None;
    } in
    t.keep_me <- Some (
      input_signal |> React.S.map (fun nodes ->
        (* print_endline "\n== update ==\n"; *)
        let old_widgets = !(t.widgets) in
        t.widgets := D.Id_map.empty;
        update t ~old_widgets nodes |> set_root_widgets;

        (*
        old_widgets |> D.Id_map.iter (fun k old ->
          if not (D.Id_map.mem k !(t.widgets)) then (
            Printf.printf "Removed unused widget '%s'\n" (D.Item.show (React.S.value old.W.item))
          )
        )
      *)
      )
    );
    t

  let widgets t = t.root_widgets

(*
    |> React.S.map ~eq:(M.equal View.eq) (M.map output)
    |> NodeList.make
*)
end
