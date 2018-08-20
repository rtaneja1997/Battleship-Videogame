open Game
open Command
open Ai
open Gui

(* [update_gui st st'] updates the GUI to display the appropriate changed data
   of state [st'], given the previous state [st]. *)
let update_gui st st' =
  if not (Game.ships_placed st) then
    let not_mem s = not (Game.p1_ships st |> List.mem s) in
    match Game.p1_ships st' |> List.find_opt not_mem with
    | None -> ()
    | Some s -> coords s |> Gui.draw_ship 70
  else
    let hit = Game.hit_success st' in
    match Game.current_turn st, Game.position_hit st' with
    | Game.P1, Some coord -> Gui.player_attack 70 coord hit
    | Game.P2, Some coord when hit -> Gui.ship_hit 70 coord
    | _ -> ()


(* [print_ai_attack st] prints the tile that the AI attacked in state [st].
 * requires: It was the AI's turn in the state before [st]. *)
let print_ai_attack st =
  match Game.position_hit st with
  | None -> ()
  | Some (r, c) -> print_endline ("The AI is attacking tile ("
                                  ^ (string_of_int r) ^ ", "
                                  ^ (string_of_int c) ^ ").")

(* [evaluate state command turn] is the REPL that updates the game based on the
 * current [state], [command], and player [turn]. *)
let rec evaluate state command =
  let turn = Game.current_turn state in
  if command.instruction = "quit" then ()
  else let state' =
    begin match turn with
      | Game.P1 when Game.ships_placed state -> Game.do' state command
      | Game.P1 -> Game.do' state command
      | _ -> Ai.make_move state
    end
    in
    if turn = Game.P2 then print_ai_attack state' else ();
    print_endline(Game.get_message state');
    update_gui state state';
    (if Game.current_turn state' = Game.P1
    then (print_string  "> "; Command.parse(read_line ()))
    else command)
    |> evaluate state'

(* [play_game gtype] initializes a game of type [gtype] and begins the ship
 * placing phase. *)
let play_game gtype =
  let ng_state = Game.new_game gtype in
  let ng'_state = Game.init_state ng_state 10 10 |> Game.ai_board in
  Gui.standard_board ();
  print_endline
  begin
    "You will be playing a "^gtype^" game against an AI. Before the game starts,
  you must place your 5 ships on the 10 x 10 board.

  You have the following ships:
  SUBMARINE: size = 3
  CRUISER: size = 4
  AIRSHIP: size = 5
  TANKER: size = 7
  BATTLESHIP: size = 8

  To place a ship, please enter the command:
  PLACE <ship_name> at r c <orientation>
  where r = row number and c = col number, and orientation = VERTICAL or HORIZONTAL.
  Placing a ship at (r,c) horizontally sets the left side of the ship at (r,c).
  Placing a ship at (r,c) vertically sets the bottom side of the ship at (r,c).
  Row and column numbers start at 0 and end at 9.

  To see which ships have been placed, you can either reference the window
  or input the command 'display'."
  end;
  print_string  "> ";
  let command = Command.parse (read_line ()) in
  evaluate ng'_state command

(* [start_game ()] prompts the player for a game mode and begins the game. *)
let rec start_game () =
  print_endline "We offer two modes of play. A STANDARD game is a normal game.
  An ADVANCED game sets the number of moves you can use equal to the number
  of ships remaining on your board. Which game would you like to play: STANDARD
  or ADVANCED?\n";
  print_string  "> ";

  let input = String.lowercase_ascii (read_line ()) in
  if input = "standard" || input = "advanced" then play_game input
  else if input = "quit" then ()
  else (print_endline "You did not enter a valid mode.\n"; start_game ())

let main () =
  begin
    try
      ANSITerminal.(print_string [blue] "\n\nWelcome to Battleship.\n");
      start_game ()
    with _ -> ()
  end;
  ANSITerminal.(print_string [blue] "\n\nThanks for playing!\n\n");
  exit 0

let () = main ()
