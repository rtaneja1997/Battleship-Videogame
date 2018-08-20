(* Module for the game and its state. *)
open Command

(* Representation type for a tile on the board. *)
type tile
(* Representation type for the game board. *)
type board = tile list list
(* Representation type for a player. NoPlayer is used when the game is not_mem
 * ongoing. *)
type player = P1 | P2 | NoPlayer
(* Representation type for a ship and its data. *)
type ship_data
(* Representation type for the state of a game. *)
type game_state

(* [new_game g] is the state of a new_game in game mode [g]. Depending on [g],
 * the game can be a STANDARD or ADVANCED game.
 * STANDARD: normal battleship game with a 10x10 board, 5 ships
 * ADVANCED: battleship game with a 10x10 board, 5 ships, num_moves = num_ships
 * left on board*)
val new_game : string -> game_state

(* [init_state st r c] is the state [st] with both players' boards set to [r]
 * rows and [c] columns. *)
val init_state : game_state -> int -> int -> game_state

(* [get_message st] is the message to be printed in state [st]. *)
val get_message : game_state -> string

(* [current_turn st] is the player whose turn it currently is in state [st]. *)
val current_turn : game_state -> player

(* [player_board st] is the current player's board in state [st].
 * raises: "Exception: game is not ongoing" if the game is not ongoing. *)
val player_board : game_state -> board

(* [opposing_board st] is the opposing player's board in state [st].
 * raises: "Exception: game is not ongoing" if the game is not ongoing. *)
val opposing_board : game_state -> board

(* [ships_placed st] is true if all ships have been placed in state [st] and
 * false otherwise. *)
val ships_placed : game_state -> bool

(* [p1_ships st] is a list of player one's ships and their coordinates in
 * state [st]. *)
val p1_ships : game_state -> ship_data list

(* [hit_success st] is true if there was a successful hit in state [st] and
 * false otherwise. *)
val hit_success : game_state -> bool

(* [position_hit st] is [Some (r, c)] if position [(r, c)] was attacked in state
 * [st] and [None] otherwise. *)
val position_hit : game_state -> (int * int) option

(* [is_hit (r, c) b] is true if the tile at position [(r, c)] in board [b] has
 * been hit and false otherwise. Returns false if [(r, c)] is not a valid
 * position. *)
val is_hit : (int * int) -> board -> bool

(* [is_ship_hit (r, c) b] is true if the tile at position [(r, c)] in board
 * [b] has been hit and contains a ship, and false otherwise. Returns false
 * if [(r, c)] is not a valid position. *)
val is_ship_hit : (int * int) -> board -> bool

(* [coords s] is a pair containing the beginning and end coordinates of the
 * ship [s]. *)
val coords : ship_data -> (int * int) * (int * int)

(* [ai_board st] is the state [st] with the AI's ships placed and the turn set to
 * player 1. The process of ship placement is randomly generated. *)
val ai_board : game_state -> game_state

(* [attack r c st] is the state that results from attacking the current player's
 * board at position [(r, c)] in state [st].
 * Precondition: (r,c) is a valid coordinate.
 *
 * If the tile is already hit, returns [st] with error message.
 * If the tile is not hit and has no ship, returns [st] with a successful hit
 * message.
 * If the tile is not hit and has a ship, returns [st] with message informing
 * the player that a ship has been hit.
 *
 * At the end of a successful hit (for a standard game), the current turn is
 * swapped (P2 if P1, P1 if P2).
 * If the type of game in st is an advanced game, attack must only progress the
 * turn once a player takes a number of moves equivalent to the number of their
 * ships left remaining on the board. *)
val attack : int -> int -> game_state -> game_state

(* [do' c st] is [st'] if doing command [c] in state [st] results
 * in a new state [st'].
 *   - For each of the valid commands, we check
          1) if a valid command is followed by other words. For example ...

            <ATTACK 2 3 otherword>

          ... is not a valid command, as ATTACK should only be followed by two
              integers. In cases like these, a message informs the player that
              the command inputted has too many words.


          2) if a command has valid arguments. For example, the place command
              must be writte as

                PLACE <shipname> at <row> <column> <orientation>


              If a player puts in too few or too many arguments, he is notified.
              Likewise, if a player puts in an incorrect shipname, or does not
              put in a valid row, column or orientation, he is notified as well.
              If multiple fields are erroneous, the error pertaining to the
              leftmost field is indicated.

    -Input is case insensitive: ATTACK 1 2, attack 1 2, and AttaCk 1 2 are all
    valid.

    - If a command isn't valid, a player is notified and prompted to reenter a
    valid command.
 *)
val do' : game_state -> Command.command -> game_state
