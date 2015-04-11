open OUnit
open Lwt

(* let () = Log.(set_log_level INFO) *)

let () = Random.self_init ()

module Queue = Lwt_pqueue.Make(struct
  type t = (float * unit Lwt.u)
  let compare a b =
    compare (fst a) (fst b)
end)

let debug fmt = Printf.ksprintf ignore fmt

module Test_clock = struct
  let schedule = ref Queue.empty
  let time = ref 0.0

  let now () = !time
  let sleep delay =
    assert (delay >= 0.0);
    let result, waker = Lwt.task () in
    schedule := !schedule |> Queue.add (!time +. delay, waker);
    result

  let async ~name f =
    let (_ : unit Lwt.t) =
      catch (fun () -> sleep 0.0 >>= f)
        (fun ex ->
          Printf.printf "Async error from '%s': %s\n" name (Printexc.to_string ex);
          exit 1) in
    ()

  let rec run_to t =
    debug "run_to %.2f\n" t;
    match Queue.lookup_min !schedule with
    | Some (wake_time, w) when wake_time <= t ->
        schedule := !schedule |> Queue.remove_min;
        time := wake_time;
        debug "time = %.2f (waking)\n" !time;
        Lwt.wakeup w ();
        run_to t
    | _ ->
        time := t;
        debug "time = %.2f\n" !time

  let reset () =
    schedule := Queue.empty;
    time := 0.0
end

module Key = struct
  module Id = Ck_id
  type t = Ck_id.t * string
  let id = fst
  let show = snd
  let compare a b =
    match compare (snd a) (snd b) with
    | 0 -> compare (fst a) (fst b)
    | r -> r
end

(*
module Git = Git_storage.Make(Irmin.Basic(Irmin_unix.Irmin_git.FS)(Irmin.Contents.String))
let _ = Unix.system "rm -rf /tmp/test_db/.git"
let () = Irmin_unix.install_dir_polling_listener 1.0
let config = Irmin_unix.Irmin_git.config ~root:"/tmp/test_db" ()
*)
module Git = Git_storage.Make(Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String))
let config = Irmin_mem.config ()

let task s =
  let date = Test_clock.now () |> Int64.of_float in
  Irmin.Task.create ~date ~owner:"User" s

module ItemMap = Map.Make(Key)
module Slow = Slow_set.Make(Test_clock)(Key)(ItemMap)
module M = Ck_model.Make(Test_clock)(Git)(struct type t = unit end)
module W = M.Widget
module Rev = Ck_rev.Make(Git)
module Up = Ck_update.Make(Git)(Test_clock)(Rev)

let format_list l = "[" ^ (String.concat "; " l) ^ "]"

type node = N of string * node list
let n name children = N (name, children)

let name_of widget =
  match W.item widget with
  | `Item item ->
      let item = React.S.value item in
      M.Item.name item
  | `Group name -> name

let rec get_tree rl =
  ReactiveData.RList.value rl
  |> List.map (fun widget ->
    let name = name_of widget in
    let children = get_tree (W.children widget) in
    let str =
      match W.state widget |> React.S.value with
      | `New -> "+" ^ name
      | `Init -> "@" ^ name
      | `Removed _ -> "-" ^ name
      | `Current -> name
    in
    N (str, children)
  )

let expect_tree s =
  match React.S.value s with
  | `Process rl | `Work rl -> rl
  | _ -> assert false

let expect_area = function
  | `Area _ as x -> x
  | _ -> assert false

let expect_some = function
  | None -> assert false
  | Some x -> x

let assert_tree ?label expected actual =
  let msg =
    match label with
    | Some l -> l
    | None -> Ck_time.string_of_unix_time !Test_clock.time in
  let rec printer items = items
    |> List.map (fun (N (name, children)) -> name ^ "(" ^ printer children ^ ")")
    |> String.concat ", " in
  debug "Expecting: %s\n" (printer expected);
  let actual =
    match get_tree actual with
    | N (("Problems" | "@Problems"), []) :: xs -> xs   (* Remove Problems if empty, as the GUI does *)
    | xs ->xs in
  assert_equal ~msg ~printer expected actual

let rec lookup path widgets =
  match path with
  | [] -> assert false
  | (p::ps) ->
      let step =
        try
          ReactiveData.RList.value widgets
          |> List.find (fun widget ->
            name_of widget = p
          )
        with Not_found -> Ck_utils.error "Node '%s' not found" p in
      match ps with
      | [] ->
          begin match W.item step with
          | `Item s -> React.S.value s
          | `Group label -> Ck_utils.error "Not an item '%s'" label end;
      | ps -> lookup ps (W.children step)

let run_with_exn fn =
  try
    fn () |> Lwt_main.run
  with ex ->
    Printexc.print_backtrace stderr;
    raise ex

let expect_action item =
  match item with
  | `Action _ as x -> x
  | _ -> assert_failure "Not an action!"

let expect_project item =
  match item with
  | `Project _ as x -> x
  | _ -> assert_failure "Not a project!"

let day d = Ck_time.make ~year:2015 ~month:4 ~day:d

let assert_str_equal = assert_equal ~printer:(fun x -> x)

let random_state ~random repo =
  let rand_int n = Random.State.int random n in
  let rand_string n = string_of_int (rand_int n) in
  let rand_time n = float_of_int (rand_int n) in

  let choose options =
    options.(rand_int (Array.length options)) in

  Git.Repository.empty repo >>= fun s ->

  (* (create objects in order, due to https://github.com/mirage/irmin/issues/190) *)
  let make_upto n dir fn =
    let rec aux i =
      if i = n then return ()
      else if rand_int 2 = 0 then aux (i + 1)
      else (
        let uuid = string_of_int i in
        let str = fn
          ~uuid:(Ck_id.of_string uuid)
          ~name:("n-" ^ rand_string 10)
          ~description:(rand_string 10)
          ~ctime:(rand_time 10) in
        Git.Staging.update s [dir; uuid] str >>= fun () ->
        aux (i + 1)
      ) in
    aux 0 in

  let random_contact ~uuid:_ ~name ~description ~ctime =
    Ck_disk_node.make_contact ~name ~description ~ctime ()
    |> Ck_disk_node.contact_to_string in

  let random_context ~uuid:_ ~name ~description ~ctime =
    Ck_disk_node.make_context ~name ~description ~ctime ()
    |> Ck_disk_node.context_to_string in

  let areas = ref [Ck_id.root] in
  let projects = ref [] in
  let random_apa ~uuid ~name ~description ~ctime =
    begin match rand_int 3 with
    | 0 ->
        let parent = choose (Array.of_list !areas) in
        areas := uuid :: !areas;
        Ck_disk_node.make_area ~name ~description ~ctime ~parent ()
    | 1 ->
        let parent = choose (Array.of_list (!areas @ !projects)) in
        projects := uuid :: !projects;
        Ck_disk_node.make_project ~name ~description ~ctime ~state:(choose [| `Active; `SomedayMaybe |]) ~parent ()
    | _ ->
        let parent = choose (Array.of_list (!areas @ !projects)) in
        Ck_disk_node.make_action ~name ~description ~ctime ~state:(choose [| `Next; `Waiting; `Future |]) ~parent ()  (* XXX *)
    end
    |> Ck_disk_node.to_string in

  make_upto (rand_int 3) "contact" random_contact >>= fun _contacts ->
  make_upto (rand_int 3) "context" random_context >>= fun _contexts ->
  make_upto (rand_int 5) "db" random_apa >>= fun _nodes ->
  return s

let suite = 
  "cue-keeper">:::[
    "delay_rlist">:: (fun () ->
      Test_clock.reset ();
      let src, set_src = React.S.create ~eq:(ItemMap.equal (=)) ItemMap.empty in
      let set items =
        List.fold_left (fun acc i ->
          ItemMap.add i i acc
        ) ItemMap.empty items
        |> set_src in
      let rename o n =
        React.S.value src
        |> ItemMap.remove o
        |> ItemMap.add n n
        |> set_src in

      let eqd rl expected =
        (* Printf.printf "Expecting: %s\n" (String.concat ", " expected); *)
        let actual =
          ItemMap.fold (fun _k item acc ->
            let (_id, b) = Slow_set.data item in
            let s = match React.S.value (Slow_set.state item) with
              | `New -> "+" ^ b
              | `Init -> "@" ^ b
              | `Current -> b
              | `Removed _ -> "-" ^ b in
            s :: acc
          ) (React.S.value rl) []
          |> List.rev in
        let msg = Printf.sprintf "t=%.2f" !Test_clock.time in
        assert_equal ~msg ~printer:format_list expected actual in
      let dst = Slow.make ~eq:(=) ~delay:1.0 src in
      eqd dst [];

      let n name =
        let id = float_of_string name *. 10. |> Printf.sprintf "%g" |> Ck_id.of_string in
        (id, name) in

      let a0  = n "0" in
      let a1  = n "1" in
      let a15 = n "1.5" in
      let a2  = n "2" in
      let a3  = n "3" in
      let a35 = n "3.5" in
      let a4  = n "4" in

      set [a1];
      eqd dst ["+1"];

      set [];
      eqd dst ["-1"];
      Test_clock.run_to 2.0;
      eqd dst [];

      set [a1; a2; a3];
      set [a1; a3];   (* Remove 2 at t=2.0 *)
      eqd dst ["+1"; "-2"; "+3"];

      set [a0; a1; a15; a3];
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"];

      set [a0; a1; a15; a3; a35];
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];
      Test_clock.run_to 2.1;
      eqd dst ["+0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];

      set [a1; a15; a3; a35];
      eqd dst ["-0"; "+1"; "+1.5"; "-2"; "+3"; "+3.5"];
      Test_clock.run_to 3.0;
      eqd dst ["-0"; "1"; "1.5"; "3"; "3.5"];
      Test_clock.run_to 3.1;
      eqd dst ["1"; "1.5"; "3"; "3.5"];

      set [a1; a15; a3];
      eqd dst ["1"; "1.5"; "3"; "-3.5"];
      Test_clock.run_to 4.1;
      eqd dst ["1"; "1.5"; "3"];

      set [a15; a3];
      eqd dst ["-1"; "1.5"; "3"];
      Test_clock.run_to 5.1;
      eqd dst ["1.5"; "3"];

      set [a0; a3];
      eqd dst ["+0"; "-1.5"; "3"];

      Test_clock.run_to 6.1;
      eqd dst ["0"; "3"];

      set [a4];
      eqd dst ["-0"; "-3"; "+4"];
      Test_clock.run_to 7.1;
      eqd dst ["4"];

      set [a0; a1; a2; a3];
      Test_clock.run_to 10.0;
      eqd dst ["0"; "1"; "2"; "3"];

      let two = (fst a2, "two") in
      rename a2 two;
      eqd dst ["0"; "1"; "-2"; "3"; "+two"];
      rename a1 (fst a1, "1.5");
      eqd dst ["0"; "1.5"; "-2"; "3"; "+two"];
      rename a3 (fst a3, "2.5");
      eqd dst ["0"; "1.5"; "-2"; "2.5"; "+two"];

      Test_clock.run_to 12.0;
      eqd dst ["0"; "1.5"; "2.5"; "two"];
    );

    "model">:: (fun () ->
      run_with_exn begin fun () ->
        Test_clock.reset ();
        let run_to_day d =
          day d |> Ck_time.unix_time_of |> Test_clock.run_to in
        let wait s =
          Test_clock.run_to (!Test_clock.time +. s) in
        run_to_day 0;
        Git.make config task >>= M.make >>= fun m ->
        M.set_mode m `Process;
        let process_tree = M.tree m |> expect_tree in
        let work = lookup ["Work"] process_tree |> expect_area in

        M.set_mode m `Work;
        let next_actions = M.tree m |> expect_tree in

        M.add_action m ~state:`Next ~parent:work ~name:"Write unit tests" () >>= fun _ ->

        (* Initially, we have a single Next action *)
        next_actions |> assert_tree ~label:"start" [
          n "@Next actions" [
            n "@Reading" [
              n "@Start using CueKeeper" [
                n "@Read wikipedia page on GTD" []
              ];
            ];
            n "+(no context)" [
              n "@Work" [
                n "@Write unit tests" []
              ]
            ];
          ];
          n "@Recently completed" []
        ];

        let read = lookup ["Next actions"; "Reading"; "Start using CueKeeper"; "Read wikipedia page on GTD"] next_actions in
        let start_using_ck = lookup ["Next actions"; "Reading"; "Start using CueKeeper"] next_actions |> expect_project in
        let units = lookup ["Next actions"; "(no context)"; "Work"; "Write unit tests"] next_actions |> expect_action in

        (* After changing it to Waiting, it disappears from the list. *)
        M.set_action_state m units `Waiting >>= fun () ->
        M.delete m read >>= function
        | `Error x -> failwith x
        | `Ok () ->
        next_actions |> assert_tree ~label:"waiting" [
          n "@Problems" [
            n "+Active project with no next action" [
              n "@Start using CueKeeper" []
            ];
          ];
          n "@Next actions" [
            n "-Reading" [
              n "@Start using CueKeeper" [
                n "@Read wikipedia page on GTD" []
              ];
            ];
            n "-(no context)" [
              n "@Work" [
                n "@Write unit tests" []
              ]
            ];
          ];
          n "@Recently completed" [];
        ];
        M.set_project_state m start_using_ck `SomedayMaybe >>= fun () ->
        wait 2.0;
        next_actions |> assert_tree ~label:"empty" [
          n "Next actions" [];
          n "Recently completed" [];
        ];

        M.add_action m ~state:`Next ~parent:work ~name:"GC unused signals" () >>= function
        | None | Some (`Area _ | `Project _) -> assert false
        | Some (`Action _ as gc) ->
        M.add_context m ~name:"Coding" () >>= function
        | None -> assert false
        | Some coding ->
        M.set_context m gc coding >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->
        M.set_context m units coding >>= function
        | `Error msg -> assert_failure msg
        | `Ok () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "+Coding" [
              n "@Work" [
                n "@GC unused signals" [];
              ]
            ];
            n "-(no context)" [
              n "@Work" [
                n "@GC unused signals" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];

        (* Get the updated units. *)
        let live_units = M.details m units in
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        assert (M.Item.action_state units <> `Next);
        (* Changing back to Next makes it reappear *)
        M.set_action_state m units `Next >>= fun () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "+Coding" [
              n "@Work" [
                n "@GC unused signals" [];
                n "+Write unit tests" []
              ];
            ];
            n "-(no context)" [
              n "@Work" [
                n "@GC unused signals" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];

        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units `Waiting >>= fun () ->
        wait 2.0;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Work" [
                n "GC unused signals" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units (`Waiting_until (day 5)) >>= fun () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Work" [
                n "GC unused signals" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        run_to_day 5;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Work" [
                n "GC unused signals" [];
                n "+Write unit tests" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_action_state m units (`Waiting_until (day 6)) >>= fun () ->
        M.add_action m ~state:(`Waiting_until (day 7)) ~parent:work ~name:"Implement scheduing" () >>= fun _ ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Work" [
                n "GC unused signals" [];
                n "-Write unit tests" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        run_to_day 6;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Work" [
                n "GC unused signals" [];
                n "+Write unit tests" [];
              ]
            ]
          ];
          n "Recently completed" [];
        ];

        run_to_day 7;
        wait 0.5;
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Work" [
                n "GC unused signals" [];
                n "Write unit tests" [];
              ]
            ];
            n "+(no context)" [
              n "@Work" [
                n "@Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];

        (* Renaming a group *)
        M.set_name m work "Dev" >>= fun () ->
        next_actions |> assert_tree [
          n "Next actions" [
            n "Coding" [
              n "Dev" [
                n "GC unused signals" [];
                n "Write unit tests" [];
              ]
            ];
            n "+(no context)" [
              n "Dev" [
                n "@Implement scheduing" [];
              ]
            ];
          ];
          n "Recently completed" [];
        ];

(*
        (* Rename conflict (e.g. two edits in different tabs *)
        let units = React.S.value (live_units.M.details_item) |> expect_some |> expect_action in
        M.set_name m units "Test conflicts" >>= fun () ->
        M.set_name m units "Fix merging" >>= fun () ->
*)

        return ()
      end
    );

    "time">:: (fun () ->
      let open Ck_time in
      let assert_less b a =
        assert_equal (compare b (of_tm (tm_of b))) 0;
        assert_equal (compare b (of_unix_time (unix_time_of b))) 0;
        assert_equal (compare b b) 0;
        assert_equal (compare a b) (-1);
        assert_equal (compare b a) 1 in
      make ~year:2000 ~month:10 ~day:5 |> assert_less (make ~year:2001 ~month:10 ~day:5);
      make ~year:2000 ~month:10 ~day:5 |> assert_less (make ~year:2000 ~month:11 ~day:5);
      make ~year:2000 ~month:10 ~day:5 |> assert_less (make ~year:2000 ~month:10 ~day:6);
      make ~year:2000 ~month:11 ~day:1 |> assert_less (make ~year:2000 ~month:10 ~day:33);

      make ~year:2015 ~month:04 ~day:9 |> string_of_user_date |> assert_str_equal "2015-05-09 (Sat)";
      let apr30 = make ~year:2015 ~month:4 ~day:0 in
      let may1st = make ~year:2015 ~month:4 ~day:1 in

      let rep = make_repeat 2 Day ~from:(make ~year:2015 ~month:3 ~day:9) in
      rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-04-09 (Thu)";
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-04-11 (Sat)";
      next_repeat rep ~now:apr30 |> string_of_user_date |> assert_str_equal "2015-05-01 (Fri)";
      next_repeat rep ~now:may1st |> string_of_user_date |> assert_str_equal "2015-05-03 (Sun)";

      let rep = make_repeat 1 Week ~from:(make ~year:2015 ~month:3 ~day:9) in
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-04-16 (Thu)";

      let rep = make_repeat 3 Month ~from:(make ~year:2014 ~month:11 ~day:30) in
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-03-30 (Mon)";
      let rep = make_repeat 2 Month ~from:(make ~year:2014 ~month:11 ~day:30) in
      next_repeat rep ~now:rep.repeat_from |> string_of_user_date |> assert_str_equal "2015-03-02 (Mon)";

      let rep = make_repeat 10 Year ~from:(make ~year:2000 ~month:2 ~day:1) in
      next_repeat rep ~now:apr30 |> string_of_user_date |> assert_str_equal "2020-03-01 (Sun)";
    );

    "merging">:: (fun () ->
      let seed = Random.int (1 lsl 28) in
      let random = Random.State.make [| seed |] in
      try
        run_with_exn begin fun () ->
          let rec aux = function
            | 0 -> return ()
            | i ->
                Git.make config task >>= fun repo ->
                let commit ~parents msg =
                  random_state ~random repo >>= fun s ->
                  Git.Commit.commit ~parents s ~msg >>= fun commit ->
                  Git.Repository.branch repo ~if_new:(lazy (return commit)) msg >>= fun _branch ->
                  return commit in
                commit ~parents:[] "base" >>= fun base ->
                commit ~parents:[base] "theirs" >>= fun theirs ->
                commit ~parents:[base] "ours" >>= fun ours ->
                Up.merge ~base ~theirs ours >>= fun result ->
                ignore result;
                aux (i - 1) in
          aux 100
        end
      with ex ->
        Printf.printf "[ random seed = %d ]\n" seed;
        raise ex
    );
  ]

let is_error = function
  | RFailure _ | RError _ -> true
  | _ -> false

let () =
  Printexc.record_backtrace true;
  let results = run_test_tt_main suite in
  Format.print_newline ();
  if List.exists is_error results then exit 1
