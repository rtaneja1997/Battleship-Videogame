(* Module for the GUI. *)
open Graphics

(* [standard_board ()] creates a new graphics window with a standard BattleShip
 * board. *)
val standard_board : unit -> unit

(* Draws the result of attacking the tile at (row,col).
 If the player hit a ship, marks the tile with a blue X
  Else, if the player misses, marks the tile with a green O. *)
val player_attack : int -> int * int -> bool -> unit

(* [ship_hit width (row, col)] indicates that the piece of the player's ship at
 * [(row,col)] has been hit by marking the tile with a red triangle of [width]. *)
val ship_hit : int -> int * int -> unit

(* [draw_ship w (start_pos, end_pos)] draws the ship starting at tile
 * [start_pos] and ending at tile [end_pos]. [w] is the width of a box on the
 * grid (for our purposes, [w] = 70 when we utilize this function) *)
val draw_ship : int -> (int * int) * (int * int) -> unit
