open OUnit2
open Game

module S_Map = Map.Make(String)
let remove k m = S_Map.remove k m
let mem k m = S_Map.mem k m
let add k v m = S_Map.add k v m
let find k m = S_Map.find k m
let empty = S_Map.empty

let j = Yojson.Basic.from_file "tworoom.json"
let init= j |> init_state

let room_1 =
  {description="This is Room 1.  There is an exit to the north.\nYou should drop the white hat here.";
   points=1; exits= add "north" "room2" empty;
   treasure=add "white hat" 1 (add "red hat" 1 empty);}
let room_2 =
  {description="This is Room 2.  There is an exit to the south.\nYou should drop the black hat here.";
   points=10; exits= add "south" "room1" empty;
   treasure=add "black hat" 1 empty;}
let r_d = add "room1" room_1 (add "room2" room_2 empty)

let b_hat= {description="a black fedora";points= 100;}
let w_hat= {description="a white panama";points= 1000;}
let r_hat= {description="a red fez";points= 10000;}
let i_d = add "black hat" b_hat (add "white hat" w_hat (add "red hat" r_hat empty))

(* Initial state*)
let init_tests =
[
  "max"   >:: (fun _ -> assert_equal 11111 (init |> max_score));
  "cur_r" >:: (fun _ -> assert_equal "room1" (current_room_id init));
  "s_inv" >:: (fun _ -> assert_equal (inv init) ["white hat"]);
  "s_vis" >:: (fun _ -> assert_equal (visited init) ["room1"]);
  "score" >:: (fun _ -> assert_equal (score init) 10001);
  "turns" >:: (fun _ -> assert_equal (turns init) 0);
  "r_d"   >:: (fun _ -> assert_equal init.rooms r_d);
  "items" >:: (fun _ -> assert_equal init.items i_d);
  "s_loc" >:: (fun _ -> assert_equal (locations init) [("black hat","room1");("red hat","room1")]);
]

(* Note : my do' is guaranteed to take only lowercase, trimmed strings from
 * the repl loop.*)
let do'_tests =
  let t_r = do' "take red hat" init in
  let t_r_d_r = do' "drop red hat" t_r in
  let t_b = do' "take black hat" init in
  let t_b_d_w = do' "drop white hat" t_b in
  let t_rb = do' "take black hat" t_r in
  let t_br = do' "take red hat" t_b in
  let b_r2 = do' "north" t_b in
[
  (* Static commands*)
  "look"      >:: (fun _ -> assert_equal init (do' "look" init));
  "inv"       >:: (fun _ -> assert_equal init (do' "inv" init));
  "inventory" >:: (fun _ -> assert_equal init (do' "inventory" init));
  "turns"     >:: (fun _ -> assert_equal init (do' "turns" init));
  "score"     >:: (fun _ -> assert_equal init (do' "score" init));
  "inspect"   >:: (fun _ -> assert_equal init (do' "inspect white hat" init));
  "quit"      >:: (fun _ -> assert_equal init (do' "quit" init));

  (*Just take another item*)
  "max"   >:: (fun _ -> assert_equal 11111 (t_b |> max_score));
  "s_inv" >:: (fun _ -> assert_equal (inv t_b) ["black hat";"white hat"]);
  "s_vis" >:: (fun _ -> assert_equal (visited t_r) ["room1"]);
  "score" >:: (fun _ -> assert_equal (score t_b) 10001);
  "turns" >:: (fun _ -> assert_equal (turns t_b) 1);
  "r_d"   >:: (fun _ -> assert_equal t_b.rooms r_d);
  "items" >:: (fun _ -> assert_equal t_b.items i_d);
  "s_loc" >:: (fun _ -> assert_equal (locations t_b) [("red hat","room1")]);

  (* Take an item out of its treasure room decreasese score*)
  "max"   >:: (fun _ -> assert_equal 11111 (t_r |> max_score));
  "cur_r" >:: (fun _ -> assert_equal "room1" (current_room_id t_r));
  "s_inv" >:: (fun _ -> assert_equal (inv t_r) ["red hat";"white hat"]);
  "s_vis" >:: (fun _ -> assert_equal (visited t_r) ["room1"]);
  "score" >:: (fun _ -> assert_equal (score t_r) 1);
  "turns" >:: (fun _ -> assert_equal (turns t_r) 1);
  "r_d"   >:: (fun _ -> assert_equal t_r.rooms r_d);
  "items" >:: (fun _ -> assert_equal t_r.items i_d);
  "s_loc" >:: (fun _ -> assert_equal (locations t_r) [("black hat","room1")]);

  (*Drop an item into its treasure room. Increases score*)
  "max"   >:: (fun _ -> assert_equal 11111 (t_r_d_r |> max_score));
  "cur_r" >:: (fun _ -> assert_equal "room1" (current_room_id t_r_d_r));
  "s_inv" >:: (fun _ -> assert_equal (inv t_r_d_r) ["white hat"]);
  "s_vis" >:: (fun _ -> assert_equal (visited t_r_d_r) ["room1"]);
  "score" >:: (fun _ -> assert_equal (score t_r_d_r) 10001);
  "turns" >:: (fun _ -> assert_equal (turns t_r_d_r) 2);
  "r_d"   >:: (fun _ -> assert_equal t_r_d_r.rooms r_d);
  "items" >:: (fun _ -> assert_equal t_r_d_r.items i_d);
  "s_loc" >:: (fun _ -> assert_equal (locations t_r_d_r) [("black hat","room1");("red hat","room1")]);

  (* Picking up two items in the room in different order produces same state*)
  "same_inv" >:: (fun _ -> assert_equal (inv t_rb) (inv t_br));
  "same_sco" >:: (fun _ -> assert_equal (score t_rb) (score t_br));
  "same_loc" >:: (fun _ -> assert_equal (locations t_rb) (locations t_br));
  "score_rb" >:: (fun _ -> assert_equal (score t_rb) 1);
  "score_b-w">:: (fun _ -> assert_equal (score t_b_d_w) 11001);

  (* Visiting a room increases the score only if the room was unvisited.
   * Adds the new room to the visited list.*)
  "move1"  >:: (fun _ -> assert_equal (score (do' "north" t_b)) 10011);
  "move2"  >:: (fun _ -> assert_equal (score (do' "go north" t_b)) 10011);
  "visited">:: (fun _ -> assert_equal (visited (do' "go north" t_b)) ["room1";"room2"]);
  "back1"  >:: (fun _ -> assert_equal (score (do' "south" b_r2)) 10011);

  "move1-1">:: (fun _ -> assert_equal (score (do' "north" t_b_d_w)) 11011);

  (* Drop the last item into its treasure room. Achieve max score*)
  "last move">:: (fun _ -> assert_equal (max_score init)
                          (score (do' "drop black hat" (do' "north" t_b_d_w))));
]

let excep_tests =
[ (* item exists but the move is illegal*)
  "can't take" >:: (fun _-> assert_raises (IllegalItem) (fun ()->(do' "take white hat" init)));
  "can't drop" >:: (fun _-> assert_raises (IllegalItem) (fun ()->(do' "drop red hat" init)));
  "can't insp" >:: (fun _-> assert_raises (IllegalItem) (fun ()->(do' "inspect red hat" init)));
  (* item does not exist*)
  "take what?" >:: (fun _-> assert_raises (Illegal) (fun ()->(do' "take blue hat" init)));
  "take?"      >:: (fun _-> assert_raises (NoItem) (fun ()->(do' "take" init)));
  "drop what?" >:: (fun _-> assert_raises (Illegal) (fun ()->(do' "drop fish" init)));
  "drop?"      >:: (fun _-> assert_raises (NoItem) (fun ()->(do' "drop" init)));
  "inspect?"   >:: (fun _-> assert_raises (Illegal) (fun ()->(do' "inspect salvador dali" init)));
  "inspect??"  >:: (fun _-> assert_raises (NoItem) (fun ()->(do' "inspect" init)));
  (* direction is not available*)
  "go here?"   >:: (fun _-> assert_raises (IllegalDir) (fun ()->(do' "go here" init)));
  "go?"        >:: (fun _-> assert_raises (NoDir) (fun ()->(do' "go" init)));
  (* Gibberish command*)
  "...what?"   >:: (fun _-> assert_raises (Illegal) (fun ()->(do' "salvador dali" init)));
  "I'm sorry?" >:: (fun _-> assert_raises (Illegal) (fun ()->(do' "awoiuncsdf" init)));
]

let suite =
  "Adventure test suite"
  >::: init_tests @ do'_tests @ excep_tests

let _ = run_test_tt_main suite
