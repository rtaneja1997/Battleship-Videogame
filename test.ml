(* OUnit test suite to test Game and Command. *)
open OUnit2
open Command
open Game

let p1 = {instruction = "place"; ship_name = "battleship";
          row_no = "0"; col_no = "0"; orientation = "horizontal";
          too_many_words = false}
let p2 = {instruction = "place"; ship_name = "cruiser";
          row_no = "4"; col_no = "2"; orientation = "vertical";
          too_many_words = false}
let a1 = {instruction = "attack"; ship_name = "";
          row_no = "0"; col_no = "0"; orientation = "";
          too_many_words = false}
let d = {instruction = "display"; ship_name = "";
         row_no = ""; col_no = ""; orientation = "";
         too_many_words = false}
let f = {instruction = "forfeit"; ship_name = "";
         row_no = ""; col_no = ""; orientation = "";
         too_many_words = false}
let t1 = {instruction = "attack"; ship_name = "";
         row_no = "0"; col_no = "0"; orientation = "";
          too_many_words = true}
let t2 = {instruction = "place"; ship_name = "submarine";
          row_no = "3"; col_no = "5"; orientation = "horizontal";
          too_many_words = true}

(* Tests for command. *)
let command_tests = [
  (* Test place ship command. *)
  "place1" >:: (fun _ ->
      assert_equal (Command.parse "place battleship at 0 0 horizontal") p1);
  "place2" >:: (fun _ ->
      assert_equal (Command.parse "plAce CRUisER at 4 2 VerTiCAL") p2);
  (* Test attack command. *)
  "attack1" >:: (fun _ -> assert_equal (Command.parse "attack 0 0") a1);
  "attack2" >:: (fun _ -> assert_equal (Command.parse "aTtAcK 0 0") a1);
  (* Test display and forfeit commands. *)
  "display" >:: (fun _ -> assert_equal (Command.parse "display") d);
  "forfeit" >:: (fun _ -> assert_equal (Command.parse "forfeit") f);
  (* Test too many words. *)
  "too1" >:: (fun _ -> assert_equal (Command.parse "attack 0 0 abcdef") t1);
  "too2" >:: (fun _ ->
      assert_equal (Command.parse "place submarine at 3 5 horizontal xyz") t2);
]

(* New game state. *)
let n = Game.new_game "standard"
(* Initial game state. *)
let i = Game.init_state n 10 10
(* State after AI places ships. *)
let a = Game.ai_board i
(* State after player places one ship. *)
let s = Game.do' a {instruction = "place"; ship_name = "battleship";
                    row_no = "0"; col_no = "0"; orientation = "horizontal";
                    too_many_words = false}
(* State after player places all ships. *)
let st =
  let a = Game.do' s {instruction = "place"; ship_name = "tanker";
                      row_no = "1"; col_no = "0"; orientation = "horizontal";
                      too_many_words = false} in
  let b = Game.do' a {instruction = "place"; ship_name = "airship";
                      row_no = "2"; col_no = "0"; orientation = "horizontal";
                      too_many_words = false} in
  let c = Game.do' b {instruction = "place"; ship_name = "cruiser";
                      row_no = "3"; col_no = "0"; orientation = "horizontal";
                      too_many_words = false} in
  Game.do' c {instruction = "place"; ship_name = "submarine";
              row_no = "4"; col_no = "0"; orientation = "horizontal";
              too_many_words = false}
(* State where player 1 has attacked. *)
let temp = Game.do' st {instruction = "attack"; ship_name = ""; row_no = "0";
                        col_no = "0"; orientation = ""; too_many_words = false}
(* State where player 2 has attacked tile with a ship. *)
let att = Game.attack 0 0 temp
(* State where player 2 has attacked a tile without a ship. *)
let no_att = Game.attack 9 9 temp

(* Tests for game. *)
let game_tests = [
  (* Test [new_game]. *)
  "new_turn" >:: (fun _ -> assert_equal (Game.current_turn n) Game.NoPlayer);
  "new_ships" >:: (fun _ -> assert_equal (Game.ships_placed n) false);
  "new_p1ships" >:: (fun _ -> assert_equal (Game.p1_ships n) []);
  "new_hit" >:: (fun _ -> assert_equal (Game.hit_success n) false);
  "new_pos" >:: (fun _ -> assert_equal (Game.position_hit n) None);

  (* Test [init_state]. *)
  "init_turn" >:: (fun _ -> assert_equal (Game.current_turn i) Game.P2);
  "init_pboard1" >:: (fun _ ->
      assert_equal (Game.player_board i |> List.length) 10);
  "init_pboard2" >:: (fun _ ->
      assert_equal (Game.player_board i |> List.hd |> List.length) 10);
  "init_oboard1" >:: (fun _ ->
      assert_equal (Game.opposing_board i |> List.length) 10);
  "init_oboard2" >:: (fun _ ->
      assert_equal (Game.opposing_board i |> List.hd |> List.length) 10);
  "init_ships" >:: (fun _ -> assert_equal (Game.ships_placed i) false);
  "init_p1ships" >:: (fun _ -> assert_equal (Game.p1_ships i) []);
  "init_hit" >:: (fun _ -> assert_equal (Game.hit_success i) false);
  "init_pos" >:: (fun _ -> assert_equal (Game.position_hit i) None);

  (* Test [ai_board]. *)
  "ai_turn" >:: (fun _ -> assert_equal (Game.current_turn a) Game.P1);
  "ai_pboard1" >:: (fun _ ->
      assert_equal (Game.player_board a |> List.length) 10);
  "ai_pboard2" >:: (fun _ ->
      assert_equal (Game.player_board a |> List.hd |> List.length) 10);
  "ai_oboard1" >:: (fun _ ->
      assert_equal (Game.opposing_board a |> List.length) 10);
  "ai_oboard2" >:: (fun _ ->
      assert_equal (Game.opposing_board a |> List.hd |> List.length) 10);
  "ai_ships" >:: (fun _ -> assert_equal (Game.ships_placed a) false);
  "ai_p1ships" >:: (fun _ -> assert_equal (Game.p1_ships a) []);
  "ai_hit" >:: (fun _ -> assert_equal (Game.hit_success a) false);
  "ai_pos" >:: (fun _ -> assert_equal (Game.position_hit a) None);

  (* Test placing a ship. *)
  "ps_turn" >:: (fun _ -> assert_equal (Game.current_turn s) Game.P1);
  "ps_pboard1" >:: (fun _ ->
      assert_equal (Game.player_board s |> List.length) 10);
  "ps_pboard2" >:: (fun _ ->
      assert_equal (Game.player_board s |> List.hd |> List.length) 10);
  "ps_oboard1" >:: (fun _ ->
      assert_equal (Game.opposing_board s |> List.length) 10);
  "ps_oboard2" >:: (fun _ ->
      assert_equal (Game.opposing_board s |> List.hd |> List.length) 10);
  "ps_ships" >:: (fun _ -> assert_equal (Game.ships_placed s) false);
  "ps_p1ships" >:: (fun _ -> assert_equal (Game.p1_ships s |> List.length) 1);
  "ps_hit" >:: (fun _ -> assert_equal (Game.hit_success s) false);
  "ps_pos" >:: (fun _ -> assert_equal (Game.position_hit s) None);
  "ps_coords" >:: (fun _ ->
      assert_equal (Game.p1_ships s |> List.hd |> Game.coords) ((0, 0), (0, 7)));

  (* Test placing all ships. *)
  "pa_turn" >:: (fun _ -> assert_equal (Game.current_turn st) Game.P1);
  "pa_pboard1" >:: (fun _ ->
      assert_equal (Game.player_board st |> List.length) 10);
  "pa_pboard2" >:: (fun _ ->
      assert_equal (Game.player_board st |> List.hd |> List.length) 10);
  "pa_oboard1" >:: (fun _ ->
      assert_equal (Game.opposing_board st |> List.length) 10);
  "pa_oboard2" >:: (fun _ ->
      assert_equal (Game.opposing_board st |> List.hd |> List.length) 10);
  "pa_ships" >:: (fun _ -> assert_equal (Game.ships_placed st) true);
  "ps_p1ships" >:: (fun _ -> assert_equal (Game.p1_ships st |> List.length) 5);
  "pa_hit" >:: (fun _ -> assert_equal (Game.hit_success st) false);
  "pa_pos" >:: (fun _ -> assert_equal (Game.position_hit st) None);

  (* Test successful [attack]. *)
  "att_turn" >:: (fun _ -> assert_equal (Game.current_turn att) Game.P1);
  "att_pboard1" >:: (fun _ ->
      assert_equal (Game.player_board att |> List.length) 10);
  "att_pboard2" >:: (fun _ ->
      assert_equal (Game.player_board att |> List.hd |> List.length) 10);
  "att_oboard1" >:: (fun _ ->
      assert_equal (Game.opposing_board att |> List.length) 10);
  "att_oboard2" >:: (fun _ ->
      assert_equal (Game.opposing_board att |> List.hd |> List.length) 10);
  "att_ships" >:: (fun _ -> assert_equal (Game.ships_placed att) true);
  "att_p1ships" >:: (fun _ -> assert_equal (Game.p1_ships att |> List.length) 5);
  "att_hit" >:: (fun _ -> assert_equal (Game.hit_success att) true);
  "att_pos" >:: (fun _ -> assert_equal (Game.position_hit att) (Some (0, 0)));

  (* Test unsuccessful [attack]. *)
  "no_att_turn" >:: (fun _ -> assert_equal (Game.current_turn no_att) Game.P1);
  "no_att_pboard1" >:: (fun _ ->
      assert_equal (Game.player_board no_att |> List.length) 10);
  "no_att_pboard2" >:: (fun _ ->
      assert_equal (Game.player_board no_att |> List.hd |> List.length) 10);
  "no_att_oboard1" >:: (fun _ ->
      assert_equal (Game.opposing_board no_att |> List.length) 10);
  "no_att_oboard2" >:: (fun _ ->
      assert_equal (Game.opposing_board no_att |> List.hd |> List.length) 10);
  "no_att_ships" >:: (fun _ -> assert_equal (Game.ships_placed no_att) true);
  "no_att_p1ships" >:: (fun _ -> assert_equal (Game.p1_ships no_att |> List.length) 5);
  "no_att_hit" >:: (fun _ -> assert_equal (Game.hit_success no_att) false);
  "no_att_pos" >:: (fun _ -> assert_equal (Game.position_hit no_att) (Some (9, 9)));
]

let suite = "test suite" >::: command_tests @ game_tests

let _ = run_test_tt_main suite
