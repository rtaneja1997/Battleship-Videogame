open Command
open Random

type tile = {hit: bool; has_ship: bool;}

type player = P1 | P2 | NoPlayer

type ship_data = {ship: string; hits: int; coords: (int*int) * (int*int) }

type board = tile list list

(* State of the game. Keeps track of both players' board and ships. Also manages
 * the current turn of the game, the type of game being played (standard or
 * advanced), and other relevant data. *)
type game_state = {
  p1_board: board ;
  p2_board: board ;
  p1_ships: ship_data list ;
  p2_ships: ship_data list ;

  ongoing: bool;
  ships_placed: bool;
  turn: player;
  game_type: string;
  successful_hit: bool;
  pos_hit: (int * int) option;
  p1_moves: int;
  p2_moves: int;

  game_mssg: string;
}


let finished_placement =
  "All ships have been placed. You can now choose among the following commands.

Attacking: Your opponent has placed 5 ships on its board as well.
To attack a tile at Row r and Column c, input the command: ATTACK r c.

If r c is not a valid tile on the board, you will receive an error
message and be prompted to reattack. Your turn will not end until
you successfully attack a tile. If you hit part of a player ship
on a tile, a blue X will appear on that tile. If you miss however,
a green O will appear instead. If any of your ships are hit, a red
triangle will appear on it.

Display: You can still use this command to see which ships remain on your board.
If any of your ships are sunk, they will be gone forever.

Forfeit: You give up the game to the opposing player. Input 'forfeit' \n"


let successful_place = "Ship has been placed \n"

let new_game gtype = {
  p1_board = [];
  p2_board = [];
  p1_ships = [];
  p2_ships = [];
  ongoing = false;
  ships_placed = false;
  turn = NoPlayer;
  successful_hit = false;
  pos_hit = None;
  p1_moves=5;
  p2_moves=5;
  game_type = gtype;
  game_mssg = "New Game has been started. Please enter size of board.\n";
  }

let init_state st rows cols =
  let create_board num_rows num_cols =

    let rec inner_list cols acc =
      if cols = 0 then acc
      else inner_list (cols -1) ({hit=false; has_ship=false}::acc)
    in

    let rec create_rows rows cols acc =
      if rows = 0 then acc
      else create_rows (rows-1) cols ((inner_list cols [])::acc)
    in

    create_rows num_rows num_cols []

  in

  let board = create_board rows cols in
  { st with
    p1_board = board;
    p2_board = board;
    turn = P2;
    game_mssg = "Created board.\n";
  }

let get_message st =
  st.game_mssg

let current_turn st =
  st.turn

let player_board st =
  match st.turn with
  | P1 -> st.p1_board
  | P2 -> st.p2_board
  | NoPlayer -> failwith "Exception: game is not ongoing"

let opposing_board st =
  match st.turn with
  | P1 -> st.p2_board
  | P2 -> st.p1_board
  | NoPlayer -> failwith "Exception: game is not ongoing"

let ships_placed st =
  st.ships_placed

let p1_ships st =
  st.p1_ships

let hit_success st =
  st.successful_hit

let position_hit st =
  st.pos_hit

let is_hit (r, c) b =
  try
    let row = List.nth b r in
    let t = List.nth row c in
    t.hit
  with _ -> false

let is_ship_hit (r, c) b =
  try
    let row = List.nth b r in
    let t = List.nth row c in
    t.hit && t.has_ship
  with _ -> false

let coords s =
  s.coords

(* [player_ships st] is a list of the current player's ships in state [st].
 * raises: "Exception: game is not ongoing" if the game is not ongoing. *)
let player_ships st =
  match st.turn with
  | P1 -> st.p1_ships
  | P2 -> st.p2_ships
  | NoPlayer -> failwith "Exception: game is not ongoing"

(* [ship_size s] returns the size of the ship named [s].
 * raises: "Exception: not a valid ship" if [s] is not the name of a ship. *)
let ship_size = function
  | "submarine" -> 3
  | "cruiser" -> 4
  | "airship" -> 5
  | "tanker" -> 7
  | "battleship" -> 8
  | _ -> failwith "Exception: not a valid ship"

(* [forfeit st] is the state [st] where the game is over and player 2 wins. *)
let forfeit st =
  if not st.ongoing
  then {st with game_mssg = "Game has not started yet\n"}
  else {st with turn = NoPlayer; ongoing = false;
                game_mssg = "You forfeited. Player 2 has won."}

(* [place_ship ship_name orientation (r, c) st] is the result of placing the
 * front of ship [ship_name] at position [(r, c)] with the given [orientation].
 *
 * Player 1 places his ships on Player 2's board and vice versa. This way, a
 * player's board has the opposing player's ships, and he can attack these
 * directly in the attack phase.
 *
 * The function takes in a valid orientation and a valid coordinate, and places
 * the ship's front at this coordinate.
 * The ship's start and end coordinates are stored in st.ship_data.
 * If adding a ship causes the number of ships the player has placed to be equal
 * to the number of total ships, then placement is done. *)
let place_ship ship_name orientation (r, c) st =

  (* Maps tiles in rows [start_row] through [start_row - num] at column [col]
   * to have a ship. *)
  let rec place_vertical start_row col board num =

    (* Marks tile at [curr_row[col_no]] to have a ship. *)
    let rec map_tile curr_row col_no =
      match curr_row with
      | [] -> []
      | h::t when col_no = 0 -> {h with has_ship = true}::t
      | h::t -> h::(map_tile t (col_no-1))
    in

    if num=0 then board
    else
      match board with
      | [] -> []
      | row::below when start_row = 0 ->
        (map_tile row col)::(place_vertical start_row col below (num-1))
      | row::below -> row::(place_vertical (start_row - 1) col below num)
  in

  (* Maps tiles in row [start_row] through columns [start_col] to
   * [start_col + ship_size] to have a ship. *)
  let place_horizontal start_row start_col board ship_size =
    let rec horizontal_map line start num =
      if num=0 then line
      else
        match line with
          | [] -> []
          | col::t when start = 0 ->
            {col with has_ship = true}::(horizontal_map t start (num-1))
          | col::t -> col::(horizontal_map t (start-1) (num))
    in

    let curr_row = List.nth board start_row in
    let new_row = horizontal_map curr_row start_col ship_size in
    let rec update_board new_row pos board =
      match board with
      |[] -> []
      |row::rest -> if pos=0 then new_row::rest
                    else row::(update_board new_row (pos-1) rest)

    in

    update_board new_row start_row board

  in

  let rec has_ship ship_list name =
    match ship_list with
    | [] -> false
    | h::t -> h.ship=name || has_ship t name
  in

  (* [add_ship st ship_name r c] is the state [st] with the ship named
   * [ship_name] added to the current player's list of ships. The data stored
   * includes the start and end coordinates and the ship name. The ship itself
   * is placed on the opposing player's board (P1's ships are placed on P2's
   * board, and vice versa). *)
  let add_ship st ship_name r c =
    let start_coord = (r,c) in
    let end_coord =
      if orientation = "horizontal" then (r, c + (ship_size ship_name) - 1)
      else (r + (ship_size ship_name) - 1, c)
    in
    let hits = ship_size ship_name in
    let new_ships =
      {ship=ship_name; hits=hits; coords=(start_coord, end_coord)}::(player_ships st)
    in
    match st.turn, orientation with
    | P1, "vertical" ->
      {st with p1_ships = new_ships;
               p2_board = place_vertical r c st.p2_board (ship_size ship_name)}
    | P1, "horizontal" ->
      {st with p1_ships = new_ships;
               p2_board = place_horizontal r c st.p2_board (ship_size ship_name)}
    | P2, "vertical" -> {st with p2_ships = new_ships;
                   p1_board = place_vertical r c st.p1_board (ship_size ship_name)}
    | P2, "horizontal" -> {st with p2_ships = new_ships;
                 p1_board = place_horizontal r c st.p1_board (ship_size ship_name)}
    | _ -> failwith "Exception: game is not ongoing"
  in

  if has_ship (player_ships st) ship_name (*Ship already placed*)
    then {st with game_mssg = "This ship has already been placed\n"}

  (* This particular ship has not been placed. *)
    else let st' = add_ship st ship_name r c in
    (* Check if all ships have been placed. *)
    if (List.length st'.p1_ships) = 5
    then {st' with ongoing = true; ships_placed = true;
                   game_mssg = finished_placement}
    else {st' with game_mssg = successful_place}

(* [can_place shipname row col orientation ships] is true if a ship with name
 * [shipname] can be placed at coordinate [(row, col)] with the given
 * [orientation].
 * A ship can be placed on a coordinate if doing so will not cause it to
 * overlap (share a tile) with any ship in [ships]. *)
let rec can_place shipname row col orientation ships =
  let rec contains list_of_coords bounds =

    let within (r,c) ((rs, cs), (re, ce)) =
      if cs=ce then c=cs && r>=rs && r<=re
      else if rs=re then r=rs && c>=cs && c<=ce
      else false
    in

  match list_of_coords with
    | []-> false
    | (x,y)::rest -> within (x,y) bounds || contains rest bounds
  in

  let rec row_coords r c1 c2 =
    if c1=c2 then [(r,c1)]
    else (r,c1)::(row_coords r (c1+1) c2)
  in

  let rec col_coords c r1 r2 =
    if r1 = r2 then [(r1,c)]
    else (r1,c)::(col_coords c (r1+1) r2)
  in

  let no_overlap ship r c ship' orientation =
    let ship_coords = (if orientation="vertical"
                       then col_coords c r (r + (ship_size ship)- 1)
                       else row_coords r c (c + (ship_size ship)-1)) in
    not (contains ship_coords ship'.coords)
  in

  match ships with
    | [] -> true
    | ship_data::rest -> no_overlap shipname row col ship_data orientation
                         && can_place shipname row col orientation rest



(* [valid_coordinates row col st] is true if [(row, col)] is a valid coordinate
 * on a player_board in [st].
 * A valid coordinate [(row,col)] is an int * int tuple for which
 * 0 <= [row] <= num_rows - 1 and 0 <= [col] <= num_cols - 1. *)
let valid_coordinates row col st =

  let is_int i =
    match int_of_string i with
    |exception int_of_string -> false
    |_ -> true
  in

  let num_rows = List.length (player_board st) in
  let num_cols = List.length (List.hd (player_board st)) in

  is_int row && is_int col &&
  (let r = int_of_string row in
   let c = int_of_string col in
   r>=0 && c>=0 && r <= (num_rows-1) && c <= (num_cols-1))

(* [valid_placement row col state orientation ship] is true if placing [ship]'s
 *  tip at [(row, col)] with the given [orientation] is valid.
 * This means:
 * 1) Placing the ship does not cause any part of the ship to be off the board.
 * 2) The [(row,col)] specified is a valid coordinate on the board (must be an
 *    int in the given range). *)
let valid_placement row col st orientation shipname  =

  let num_rows = List.length (st.p1_board) in
  let num_cols = List.length (List.hd st.p1_board) in

  valid_coordinates row col st &&
  (if orientation = "vertical"
    then ((int_of_string row) + (ship_size shipname) - 1) < num_rows
    else ((int_of_string col) + (ship_size shipname) - 1) < num_cols)

(* [valid_shipname s] is true if [s] is a valid name of a ship and false
 * otherwise. *)
let valid_shipname s =
  match String.lowercase_ascii s with
  | "submarine" -> true
  | "cruiser" -> true
  | "airship" -> true
  | "tanker" -> true
  | "battleship" -> true
  | _ -> false


(* [valid_orientation o] is true if [o] is a valid orientation of a ship
 * (horizontal or vertical), and false otherwise. *)
let valid_orientation o =
  match String.lowercase_ascii o with
  | "horizontal" -> true
  | "vertical" -> true
  | _ -> false

(* [ai_place ship_name st] is the state [st] with the ship with name [ship_name]
 * placed at some random coordinate on player one's board. If the random
 * coordinate generated is not valid, or if placing a ship on the coordinate
 * would cause overlap with another ship, [ai_place] runs again to generate a
 * new coordinate. Otherwise, the ship is placed at the given coordinate with
 * the given orientation. *)
let rec ai_place ship_name st =
  Random.self_init ();
  let orientations = ["horizontal"; "vertical"] in
  let size_bound = List.length st.p1_board in

  let o = List.nth orientations (Random.int 2) in
  let (r,c) = (Random.int size_bound, Random.int size_bound) in

  if not (valid_placement (string_of_int r) (string_of_int c) st o ship_name)
  || not (can_place ship_name r c o st.p2_ships)
  then ai_place ship_name st
  else place_ship ship_name o (r,c) st

let ai_board st =
  let st' = ai_place "submarine" st |> ai_place "airship" |> ai_place "cruiser"
            |> ai_place "battleship" |> ai_place "tanker" in
  {st' with turn = P1; game_mssg = "The AI has placed its ships.\n"}

(* [update_board r c b] is board [b] with the tile at [(r, c)] marked as hit.
 * raises: Failure "nth" if [(r, c)] is not a valid position on [b]. *)
let update_board r c b =
  let new_row =
    List.nth b r |> List.mapi (fun i t -> if i = c then {t with hit = true} else t)
  in
  List.mapi (fun i row -> if i = r then new_row else row) b

(* [update_ships r c ships] is a pair containing the list [ships] with the ship
 * at position [(r, c)] marked as hit, a message indicating whether the ship was
 * sunk or merely hit, and an indicator that the tile has not alreay been hit. *)
let update_ships r c ships =
  let new_ships =
    let coord_within s =
      let ((r1, c1), (r2, c2)) = s.coords in
        if c1=c2 then r >= r1 && r<= r2 && c=c1
        else if r1=r2 then c1 <= c && c <= c2 && r=r1
        else false
    in
    ships
    |> List.map (fun s -> if coord_within s then {s with hits=s.hits - 1} else s)
    |> List.filter (fun s -> s.hits > 0)
  in
  if List.length ships = List.length new_ships
  then new_ships, "Hit! There is a ship at these coordinates.\n", true, false
  else new_ships, "Hit! A ship has been sunk!\n", true, false

(* [hit_tile r c st] is the state, already_hit that results from hitting the
 * tile at [(r, c)] on the current player's board in state [st]. The already_hit
 * value is true if a the tile was already hit.
 * raises: Failure "nth" if [(r, c)] is not a valid position on the board. *)
let hit_tile r c st =
  let (b, ships) =
    if st.turn = P1 then (st.p1_board, st.p2_ships)
    else (st.p2_board, st.p1_ships)
  in
  let new_board = update_board r c b in
  let (new_ships, m, hit, already_hit) =
    if new_board = b then (ships, "This tile has already been hit.\n", false, true)
    else if (List.nth (List.nth b r) c).has_ship then update_ships r c ships
    else (ships, "Miss! There is no ship at these coordinates.\n", false, false)
  in
  match st.turn with
  | P1 ->
    {st with p1_board = new_board; p2_ships = new_ships; game_mssg = m;
             successful_hit = hit}, already_hit
  | _ ->
    {st with p2_board = new_board; p1_ships = new_ships; game_mssg = m;
             successful_hit = hit}, already_hit

let rec attack row col st =
  let get_playermoves st =
    match st.turn with
    | P1 -> st.p1_moves
    | P2 -> st.p2_moves
    | NoPlayer -> -1
  in

  let decrement_turn st =
    match st.turn with
    | P1 when st.pos_hit <> None -> {st with p1_moves = st.p1_moves - 1}
    | P1 -> st
    | P2 -> {st with p2_moves = st.p2_moves - 1}
    | NoPlayer -> failwith "no game is ongoing\n"
  in

  let reset_moves st =
    match st.turn with
    | P1 -> {st with p1_moves = List.length st.p1_ships}
    | P2 -> {st with p2_moves = List.length st.p2_ships}
    | NoPlayer -> failwith "no game is ongoing\n"
  in

  let st', already_hit = hit_tile row col st in
  let (t, coord) =
    if already_hit then st.turn, None
    else if st.turn = P1 then P2, Some (row, col)
    else P1, Some (row, col)
  in
  if List.length st'.p1_ships = 0 then
    {st' with pos_hit = coord; ongoing = false; turn = NoPlayer;
              game_mssg = "You have lost.\n"}
  else if List.length st'.p2_ships = 0 then
    {st' with pos_hit = coord; ongoing = false; turn = NoPlayer;
              game_mssg = "You have won!\n"}

  else if st.game_type = "standard" then {st' with turn = t; pos_hit = coord}
  else
    let new_state = decrement_turn({st' with pos_hit = coord}) in
    let player_moves = get_playermoves new_state in

    if player_moves=0 then reset_moves ({st' with turn=t; pos_hit = coord})
    else
      let p = if new_state.turn = P1 then "\nYou have " else "\nThe AI has " in
      let n = if player_moves = 1 then " move" else " moves" in
      let m = p ^ (string_of_int (player_moves)) ^ n ^ " remaining.\n" in
      {new_state with game_mssg = new_state.game_mssg ^ m}

(* [display_ships st] is a message listing player one's ships in state [st]. *)
let display_ships st =
  let rec get_ships ship_list =
    match ship_list with
    | [] -> "You have no ships placed\n"
    | {ship=ship_name; hits=_; coords=((r1,c1), (r2, c2))}::[] ->
      "Ship: "^ship_name^". Starts at Row "^(string_of_int r1)^", Col "
      ^(string_of_int c1)^" and ends at Row "
      ^(string_of_int r2)^", Col "^(string_of_int c2)^"\n"
    | h::t -> get_ships [h] ^ (get_ships t)
  in
  get_ships st.p1_ships

(* [do_place st command] is the state that results from executing the place ship
 * [command] in state [st]. *)
let do_place st command =

  let too_few c =
    command.ship_name = "" || command.row_no = "" ||
    command.col_no = "" || command.orientation = ""
  in

  if st.ongoing
  then {st with game_mssg = "You cannot place a ship while the game is ongoing.\n"}
  else if too_few command
  then {st with game_mssg = "You did not input a complete command.\n"}
  (* Check if the shipname is valid. *)
  else if not (valid_shipname command.ship_name)
  then {st with game_mssg = "You did not specify a valid ship name.\n"}
  (* Check if orientation is valid. *)
  else if not (valid_orientation command.orientation)
  then {st with game_mssg = "You did not specify a valid orientation.\n "}
  (* Check if the ship can be placed. *)
  else if not (valid_placement command.row_no command.col_no
                 st command.orientation command.ship_name)
  then {st with game_mssg = "You did not specify a valid position on the board.\n"}

  else if not (can_place command.ship_name (int_of_string command.row_no)
                 (int_of_string command.col_no) command.orientation st.p1_ships)
  then let m = "You cannot place a ship here, as it would overlap " in
    {st with game_mssg = m ^ "with an already placed ship.\n"}

  else
    let row = int_of_string command.row_no in
    let col = int_of_string command.col_no in
    place_ship command.ship_name command.orientation (row,col) st

(* [do_attack st command] is the state that results from executing the attack
 * [command] in state [st]. *)
let do_attack st command =
  if not st.ongoing
  then {st with pos_hit = None; game_mssg = "Game has not started yet.\n"}
  else if not (valid_coordinates command.row_no command.col_no st)
  then {st with pos_hit = None;
                game_mssg = "You did not specify a valid row, column combination.\n"}
  else attack (int_of_string command.row_no) (int_of_string command.col_no) st

let do' st command =
  match String.lowercase_ascii command.instruction with
  | "forfeit" when command.ship_name <> "" ->
    {st with successful_hit = false; pos_hit = None; game_mssg = "Enter the word FORFEIT only.\n"}
  | "forfeit" -> forfeit st
  | "attack" when command.too_many_words ->
    {st with successful_hit = false; pos_hit = None; game_mssg = "You entered too many words for this command.\n"}
  | "attack" -> do_attack st command
  | "place" when command.too_many_words ->
    {st with successful_hit = false; pos_hit = None; game_mssg = "You entered too many words for this command\n"}
  | "place" -> do_place st command
  | "display" when command.ship_name <> "" ->
    {st with successful_hit = false; pos_hit = None; game_mssg = "Enter the word DISPLAY only.\n"}
  | "display" ->
    {st with successful_hit = false; pos_hit = None; game_mssg = display_ships st}

  | _ -> {st with successful_hit = false; pos_hit = None;
                  game_mssg = "This is not a valid command.\n"}
