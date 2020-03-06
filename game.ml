(*
 * CS 3110 Fall 2016 A2
 * Author: Dongbin (DB) Lee
 * NetID: dl654
 *
 * Acknowledge here any contributions made to your solution that
 * did not originate from you or from the course staff: Affirmed.
 *
 *)

open Yojson.Basic
open Yojson.Basic.Util
open ANSITerminal

(* My solution will use the Map module extensively:
 *   regular a' Maps as dictionaries, with one exception:
 *   int Maps are a substitute for hashtables. Every key maps to 1.
 * Define module and shortcuts.*)
module S_Map = Map.Make(String)

let remove k m = S_Map.remove k m
let mem k m = S_Map.mem k m
let add k v m = S_Map.add k v m
let find k m = S_Map.find k m
let empty = S_Map.empty

(* A room and item contains pertinent info as a record.*)
type room = {
  description: string;
  points: int;
  exits: string S_Map.t; (* This maps direction to room_ids *)
  treasure: int S_Map.t; (* Maps items to a number--in this case, 1.*)
}
type item = {description:string; points:int;}

(* A state contains all the information in the json file.
 * The first three fields are constant; others will change from calling do'.*)
type state = {
  max_score: int;
  rooms: room S_Map.t; (* Maps room to its information record *)
  items: item S_Map.t; (* Maps item to its information record *)

  score: int;
  turns: int;
  room_now: string;
  inv: int S_Map.t;       (* Hashtable of items*)
  visited: int S_Map.t;   (* Hashtable of room_ids*)
  i_loc: string S_Map.t;  (* Maps item to its location*)
  init_j: json;
}

(* [Illegal] is raised by [do'] to indicate that a command is illegal;
 * see the documentation of [do'] below. *)
exception Illegal

(* Shortcut for printing prompts*)
let p_e str = ANSITerminal.(print_string [yellow] ("\n"^str^"\n\n"))

(* The following helper functions help construct the fields of state from
 * information extracted from json*)

(* Puts a list of values into a "hashtable"*)
let list_to_hash l =
  let rec one_way_map one_l acc =
    match one_l with
    |[] -> acc
    |h::t-> add h 1 (one_way_map t acc) in
  one_way_map l empty

(* Processes two lists of values into a single list by combining values from
 * each list according to f.
 * Assumes that the the lists have same length, according to schema.json.*)
let rec two_l_to_data list_1 list_2 f acc =
  match list_1 with
  |[] -> acc
  |h1::t1 -> (
    match list_2 with
    |[] -> failwith "more things to map? oh boy"
    |h2::t2 -> f h1 h2 (two_l_to_data t1 t2 f acc))

(* Takes a list of keys and a list of values into a dictionary.*)
let lists_to_dic l1 l2 = two_l_to_data l1 l2 add empty


(* Concatenates a dictionary onto a list.*)
let cons_dic h1 h2 li = List.cons (lists_to_dic h1 h2) li

(* Takes the lists of directions and corresponding exits for each room, creates
 * an exit_dictionary, and puts it in a list sequentially *)
let exit_dic_maker dir room = two_l_to_data dir room cons_dic []


(* Given lists of room_id, description, points, exits, and treasures, first
 * create a list of room records and then create a dictionary of rooms*)
let room_dic_maker id_l de_l po_l ex_l tr_l =
    let rec room_l_maker d p e t acc =
      match d with
      |[]->acc
      |h1::t1 -> (
        match p with
        |[]-> failwith "what room"
        |h2::t2 -> (
          match e with
          |[]-> failwith "what room"
          |h3::t3 -> (
            match t with
            |[]-> failwith "what room"
            |h4::t4 -> {description=h1;points=h2;exits=h3;treasure=h4}
                      ::room_l_maker t1 t2 t3 t4 acc))) in
  lists_to_dic id_l (room_l_maker de_l po_l ex_l tr_l [])

(* Given lists of item_id, description, and points, create a list of item
 * records and then create a dictionary of items.*)
let item_dic_maker id_l de_l po_l=
    let rec item_l_maker d p acc =
      match d with
      |[]->acc
      |h1::t1 -> (
        match p with
        |[]-> failwith "what room"
        |h2::t2 -> {description=h1;points=h2;}::(item_l_maker t1 t2 acc)) in
  lists_to_dic id_l (item_l_maker de_l po_l [])

(* Shortcut for extracting fields from json files.*)
let fields json dir f =
  json |> member dir |> to_list |> filter_member f


(* [init_state j] is the initial state of the game as
 * determined by JSON object [j] *)
let init_state j =
  (* extract starting info*)
    let start_inv  = j |> member "start_inv"  |> to_list
                       |> filter_string       |> list_to_hash in
    let start_room = j |> member "start_room" |> to_string in
    (* create a dictionary of item locations*)
    let start_loc_i = fields j "start_locations" "item" |> filter_string in
    let start_loc_r = fields j "start_locations" "room" |> filter_string in
    let loc_dic     = lists_to_dic start_loc_i start_loc_r in

  (* extract info about rooms and create a dictionary of rooms*)
    let r_list  = fields j "rooms" "id"          |> filter_string in
    let d_list  = fields j "rooms" "description" |> filter_string in
    let r_point = fields j "rooms" "points"      |> filter_int in
    (* create a didctionary of exits*)
    let e_dir   = fields j "rooms" "exits" |> filter_list |>
        List.map (fun x -> x |> filter_member "direction" |> filter_string) in
    let e_rooms = fields j "rooms" "exits" |> filter_list |>
        List.map (fun x -> x |> filter_member "room_id"   |> filter_string) in
    let e_dic_l = exit_dic_maker e_dir e_rooms in
    (* create a hashtable of treasure*)
    let t_dic_l = fields j "rooms" "treasure" |> filter_list |>
      List.map (fun x->list_to_hash (filter_string x)) in

    let r_dict = room_dic_maker r_list d_list r_point e_dic_l t_dic_l in

  (* Similarly, extract info about items and create a dictionary of items*)
    let i_iden  = fields j "items" "id"          |> filter_string in
    let i_desc  = fields j "items" "description" |> filter_string in
    let i_point = fields j "items" "points"      |> filter_int in
    let i_dict  = item_dic_maker i_iden i_desc i_point in

    (* Item k is in location d. If d's treasure contain k, then add points*)
    let i_in_tr k d acc =
      let k_poin = (find k i_dict).points in
      let p_plus = if mem k (find d r_dict).treasure then k_poin else 0 in
      acc + p_plus in

    (* Starting score is the starting room's point plus the points from whatever
     * item is already in its treasure room.*)
    let score_in = (find start_room r_dict).points + (S_Map.fold i_in_tr loc_dic 0) in

    (* Max point is the sum of item points and room points.*)
    let max_points = List.fold_left (fun a b->a+b) 0 (r_point @ i_point) in

    (* Print the start_room's description.*)
    p_e (find start_room r_dict).description;

    (* Finally, output the initial state. Whew.*)
   {max_score = max_points;
    score = score_in;
    turns = 0;
    room_now = start_room;
    inv = start_inv;
    visited = add start_room 1 empty;

    rooms = r_dict;
    i_loc = loc_dic;
    items = i_dict;
    init_j = j}

(* [max_score s] is the maximum score for the adventure whose current
 * state is represented by [s]. *)
let max_score s =
  s.max_score
  (*sum all the points in each room in init_state and keep passing it on*)

(* [score s] is the player's current score. *)
let score s =
  s.score

(* [turns s] is the number of turns the player has taken so far. *)
let turns s =
  s.turns

(* [current_room_id s] is the id of the room in which the adventurer
 * currently is. *)
let current_room_id s =
  s.room_now

(* Return hashtables as lists for printing convenience. *)
let hash_to_list h =
  match List.split (S_Map.bindings h) with
  |(l,_)->l

(* [inv s] is the list of item id's in the adventurer's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant. *)
let inv s = hash_to_list s.inv

(* [visited s] is the list of id's of rooms the adventurer has visited.
 * No room may appear more than once in the list.  Order is irrelevant. *)
let visited s = hash_to_list s.visited

(* [locations s] is an association list mapping item id's to the
 * id of the room in which they are currently located.  Items
 * in the adventurer's inventory are not located in any room.
 * No item may appear more than once in the list.  The relative order
 * of list elements is irrelevant, but the order of pair components
 * is essential:  it must be [(item id, room id)]. *)
let locations s =
  S_Map.bindings s.i_loc


(* IllegalItem is raised when the said item exists but the move is illegal.*)
exception IllegalItem
exception IllegalDir
exception NoItem
exception NoDir

(* [do' c st] is [st'] if it is possible to do command [c] in
 * state [st] and the resulting new state would be [st'].  The
 * function name [do'] is used because [do] is a reserved keyword.
 *   - The "go" (and its shortcuts), "take" and "drop" commands
 *     either result in a new state, or are not possible because
 *     their object is not valid in state [st] hence they raise [Illegal].
 *       + the object of "go" is valid if it is a direction by which
 *         the current room may be exited
 *       + the object of "take" is valid if it is an item in the
 *         current room
 *       + the object of "drop" is valid if it is an item in the
 *         current inventory
 *       + if no object is provided (i.e., the command is simply
 *         the bare word "go", "take", or "drop") the behavior
 *         is unspecified
 *   - The "quit", "look", "inventory", "inv", "score", and "turns"
 *     commands are always possible and leave the state unchanged.
 *   - The behavior of [do'] is unspecified if the command is
 *     not one of the commands given in the assignment writeup.
 * The underspecification above is in order to enable karma
 * implementations that provide new commands. *)

(* My solution parses the command within do'. Hence the mess.
 * from repl, the command [c] is trimmed and in lowercase.*)
let do' c st =
  let len = String.length c in
  let n_sub n = String.sub c 0 n in

  (* the commands that return the same state: print accordingly:
   * quit, inv, score, turns. inspect.
   * inspect and look at both return the description of the item*)
  if c = "quit" then st
  else if c = "look" then
    (p_e (find st.room_now st.rooms).description; st)
  else if c = "inventory" || c = "inv" then
    (p_e "Inventory :";
     List.iter print_endline (inv st);
     p_e "You can *inspect* these items."; st)
  else if c = "score" then
    (p_e ("Score : "^(string_of_int (score st))); st)
  else if c = "turns" then
    (p_e ("Turns taken : "^(string_of_int (turns st))); st)
  else if (c = "look at" || c = "inspect") then raise NoItem
  else if (c = "take" || c = "drop") then raise NoItem
  else if c = "go" then raise NoDir

  else if len > 7 && (n_sub 7= "look at" || n_sub 7 = "inspect") then (
    let thing = String.trim (String.sub c 7 (len-7)) in
    if mem thing st.inv then (
      p_e (find thing st.items).description; st
    )
    else if mem thing st.items then raise IllegalItem
    else raise Illegal
  )
  (* active commands: changes the state*)
  else if len > 4 && (n_sub 4 = "take" || n_sub 4 = "drop") then (
    let thing = String.trim (String.sub c 4 (len-4)) in
    if mem thing st.items then (
      (* If legal, taking an item puts it in the inv, removes it from current
       * room, increments turns, and modifies score under conditions.*)
      if n_sub 4 = "take" then (
        if mem thing st.i_loc && find thing st.i_loc = st.room_now then
          let minus_i = if mem thing (find st.room_now st.rooms).treasure
                      then (find thing st.items).points else 0 in (
          p_e ("You acquired an item! : "^thing);
          {st with i_loc = remove thing st.i_loc;
                     inv = add thing 1 st.inv;
                     score = st.score - minus_i;
                     turns = st.turns + 1;}
          )
        else raise IllegalItem
      )
      (* If legal, dropping an item puts it in the current room, removes it from
       * inv, increments turns, and modifies score under conditions.*)
      else
        if mem thing st.inv then (
          let plus_i = if mem thing (find st.room_now st.rooms).treasure
                     then (find thing st.items).points else 0 in (
          p_e ("You put an item in the room! : "^thing);
          {st with i_loc = add thing st.room_now st.i_loc;
                     inv = remove thing st.inv;
                     score = st.score + plus_i;
                     turns = st.turns+1;}
          )
        )
      else raise IllegalItem
    )
  else raise Illegal
  )
  (* if c is a direction that is legal, change room_now, increment turns, add
   * points if the room is not visited, and add the room to visited.*)
  else
    let dir = (if len > 2 && n_sub 2 = "go"
               then String.trim (String.sub c 2 (len-2)) else c) in
    let go_swh = (len > 2 && n_sub 2 = "go") in (
    if mem dir (find st.room_now st.rooms).exits then
      let room_next = find dir (find st.room_now st.rooms).exits in
      let plus_r = if mem room_next st.visited then 0
                   else (find room_next st.rooms).points in (
      p_e (find room_next st.rooms).description;
      {st with room_now = room_next;
               score = st.score + plus_r;
               turns = st.turns + 1;
               visited = add room_next 1 st.visited;}
      )
    else if go_swh then raise IllegalDir
    else raise Illegal
  )

(* The main repl loop functionality. This function makes sure that do' receives
 * only lowercase, trimmed strings. If conditions are met, end the game. If
 * a move is illegal, catch it appropriately and display messages.*)
let rec repl s = try (
  let str = String.trim (String.lowercase_ascii (read_line ())) in
  let new_s = do' str s in
  if new_s.score >= new_s.max_score
    then ANSITerminal.(print_string [red] "\nYou have reached the maximum score! You won!\n\n")
  else
    let now_id= new_s.room_now in
    let now_room = S_Map.find now_id new_s.rooms in
    if now_room.exits = S_Map.empty then (
    ANSITerminal.(print_string [red]
      ("\nYou got lost and couldn't finish the game:(. Would you like to try again?\nY/N\n\n"));
    let cont = String.lowercase_ascii (read_line ()) in
    if cont = "y" || cont = "yes" then repl (init_state new_s.init_j)
    else if cont = "n" || cont = "no" then print_endline "OK."
    else print_endline "We didn't catch that. Quitting the game.")
  else if str = "quit"
    then p_e "Quitting the game."
  else repl new_s
  ) with
  | IllegalItem -> p_e "You cannot perform that move on the item..."; repl s
  | IllegalDir -> p_e "That is not a valid direction..."; repl s
  | NoItem -> p_e "You must specify an item..."; repl s
  | NoDir -> p_e "You must specify a direction..."; repl s
  | _ -> p_e "Sorry, I didn't catch that."; repl s

(* [main f] is the main entry point from outside this module
 * to load a game from file [f] and start playing it
 *)
let rec main file_name =
  try (
    let j = from_file file_name in
    let init = init_state j in
    repl init
  ) with
    (* If there is no fileffile_name in the directory, reprompt message*)
    | _ ->
      if file_name = "quit" then p_e "Quitting the game."
      else (
        print_endline "\nSomething went wrong while loading :(";
        print_endline "Please check that the file name and the file are valid.\n";
        let new_json = read_line () in
        main new_json
      )