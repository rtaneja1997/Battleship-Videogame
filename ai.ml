open Game
open Random

(* [sequence_above (r, c) b acc] is the number of adjacent vertical tiles in
 * board [b] that have been hit but do not have a ship, starting at the tile in
 * position [(r, c)] and moving up, added to [acc]. *)
let rec sequence_above (r, c) b acc =
  if Game.is_ship_hit (r, c) b
  then sequence_above (r + 1, c) b (acc + 1)
  else acc

(* [sequence_below (r, c) b acc] is the number of adjacent vertical tiles in
 * board [b] that have been hit but do not have a ship, starting at the tile in
 * position [(r, c)] and moving down, added to [acc]. *)
let rec sequence_below (r, c) b acc =
  if Game.is_ship_hit (r, c) b
  then sequence_below (r - 1, c) b (acc + 1)
  else acc

(* [sequence_left (r, c) b acc] is the number of adjacent horizontal tiles in
 * board [b] that have been hit but do not have a ship, starting at the tile in
 * position [(r, c)] and moving left, added to [acc]. *)
let rec sequence_left (r, c) b acc =
  if Game.is_ship_hit (r, c) b
  then sequence_left (r, c - 1) b (acc + 1)
  else acc

(* [sequence_right (r, c) b acc] is the number of adjacent horizontal tiles in
 * board [b] that have been hit but do not have a ship, starting at the tile in
 * position [(r, c)] and moving right, added to [acc]. *)
let rec sequence_right (r, c) b acc =
  if Game.is_ship_hit (r, c) b
  then sequence_right (r, c + 1) b (acc + 1)
  else acc

(* [max_sequence (r, c) b] is the length of the longest adjacent sequence of
 * tiles to the tile [t] at position [(r, c)] that have been hit but do not have
 * ships. A tile adjcent to [t] is a tile that is either directly above, below,
 * to the left, or to the right of [t]. An adjacent sequence of tiles is either
 * a horizontal or vertical sequence where each tile has been hit, each tile
 * does not have a ship, and one of the tiles at either end of the sequence is
 * adjacent to [t]. If [t] has already been hit, the max sequence is -1.
 * requires: [(r, c)] is a valid position in [b]. *)
let max_sequence (r, c) b =
  if Game.is_hit (r, c) b then -1
  else
    let up = sequence_above (r + 1, c) b 0 in
    let down = sequence_below (r - 1, c) b 0 in
    let left = sequence_left (r, c - 1) b 0 in
    let right = sequence_right (r, c + 1) b 0 in
    max (max up down) (max left right)

let make_move st =
  let b = Game.player_board st in
  let height = List.length b in
  let width = List.hd b |> List.length in

  (* [choose_random ()] is a random unhit coordinate on the board. *)
  let rec choose_random () =
    Random.self_init ();
    let coord = (Random.int height, Random.int width) in
    if not (Game.is_hit coord b) then coord else choose_random ();
  in

  (* [best_coord (r, c) best_pos best_seq] is the coordinate, starting from
   * position [(r, c)] and moving left to right, bottom to top, that has a
   * longer sequence than [best_seq], the sequence of the coordinate [best_pos].
   * If there is no best coordinate, returns a random unhit coordinate. *)
  let rec best_coord (r, c) best_pos best_seq =
    if r >= height && best_seq > 0 then best_pos
    else if r >= height then choose_random ()
    else
      let seq = max_sequence (r, c) b in
      let (best_pos', best_seq') =
        if seq > best_seq then (r, c), seq else best_pos, best_seq
      in
      let pos' = if c = width - 1 then (r + 1, 0) else (r, c + 1) in
      best_coord pos' best_pos' best_seq'
  in

  let (r, c) = best_coord (0, 0) (0, 0) (-1) in
  Game.attack r c st
