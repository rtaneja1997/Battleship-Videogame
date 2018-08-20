(* Module for player commands. *)

(* A value of type [command] represents a valid player command. The possible
 * commands, and the parts of the game where they are valid, are:
 *
 * VALID AT START OF GAME:
 * [PLACE <ship> AT <row> <col> <orientation: VERTICAL or HORIZONTAL>]:
 * places the front tip of ship at position (r, c) in either horizontal or
 * vertical fashion.
 *
 * VALID DURING ATTACK PHASE:
 * [ATTACK <row> <col>]: attacks the tile at the specified row and col.
 * [FORFEIT]: the game ends and the opposing player wins.
 *
 * VALID THROUGHOUT GAME:
 * [DISPLAY]: shows a list of player ships on the board. *)
type command = {instruction: string; ship_name: string; row_no: string;
                col_no: string; orientation: string; too_many_words: bool}

(* [parse str] parses the player's input [str] and returns a corresponding
 * command. *)
val parse : string -> command
