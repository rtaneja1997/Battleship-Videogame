(* Module for the game playing AI. *)
open Game

(* [make_move st] is an updated state that results from making the best possible
 * move (according to the AI's logic) in state [st]. *)
val make_move : Game.game_state -> Game.game_state
