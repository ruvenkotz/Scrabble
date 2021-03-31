open OUnit2
open Board
open Bag
open Hand
(** [board_is_empty_test name board row col] is an oUnit test named [name] which
    checks that [Board.is_empty board row col] is [expecetd_bool] *)
let board_is_empty_test name board row col expected_bool =
  name >:: fun _ -> assert_equal expected_bool (Board.is_empty board row col) 
    ~printer:Bool.to_string

(** [board_get_char_test name board row col expected_char] is an oUnit test
    named [name] which checks that the chaarcter returned by [Board.get_char 
    board row col] is indeed [expected_char] *)
let board_get_char_test name board row col expected_char =
  name >:: fun _ -> assert_equal expected_char (Board.get_char board row col)
    ~printer:Char.escaped

(** [board_get_char_raises_test name board row col expected_error] is an oUnit
    test which checks that the exception raised by
    [Board.get_char board row col] is [expected_error] *)
let board_get_char_raises_test name board row col expected_error =
  name >:: fun _ -> 
    assert_raises expected_error (fun _ -> Board.get_char board row col)

(** [board_set_char_test name board row col new_char] is an oUnit test named 
    [name] which checks that new_char gets inserted at [row] [col] of [board] *)
let board_set_char_test name board row col new_char =
  name >:: fun _ -> 
    let new_board = Board.set_char board row col new_char in
    assert_equal new_char (Board.get_char new_board row col) 
      ~printer:Char.escaped

(** [board_set_char_raises_test name board row col chr expected_error] is an
    oUnit test named [name] which checks that the error raised by 
    [Board.set_char board row col chr] is [expected_error] *)
let board_set_char_raises_test name board row col chr expected_error =
  name >:: fun _ -> 
    assert_raises expected_error (fun _ -> Board.set_char board row col chr)

(** [empty_board] is an empty board. *)
let empty_board = board_init

(** [board_builder_helper current_board counter chr_lst] iterates through each 
    character in [chr_lst], and puts it in the next available position of 
    [current_board] *)
let rec board_builder_helper current_board counter chr_lst = match chr_lst with
| [] -> current_board
| h :: t ->
  let new_board = 
    Board.set_char current_board (counter / 15) (counter mod 15) h in
  board_builder_helper new_board (counter + 1) t

(** [board_builder chr_lst] fills a board with the letters from [chr_lst], 
    starting at the top row, and going left to right. When one row gets filled, 
    the next row begins being filled, and so on. 
    Raises: PosUnknown if [chr_lst] is longer than 225 characters *)
let board_builder chr_lst = board_builder_helper Board.board_init 0 chr_lst

(** [half_board] is a board with the first 104 positions filled with letters in 
    alphabetical order. *)
let half_board : Board.t = board_builder [
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';
    'T';'U';'V';'W';'X';'Y';'Z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';
    'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'A';'B';'C';'D';'E';
    'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';
    'Y';'Z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';
    'R';'S';'T';'U';'V';'W';'X';'Y';'Z'
  ]

(** [full_board] is a board with all the spaces filled in alphabetical order *)
let full_board : Board.t = board_builder [
  'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';
  'T';'U';'V';'W';'X';'Y';'Z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';
  'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'A';'B';'C';'D';'E';
  'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';
  'Y';'Z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';
  'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
  'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'A';'B';'C';
  'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';
  'W';'X';'Y';'Z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';
  'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'A';'B';'C';'D';'E';'F';'G';'H';
  'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'A';
  'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q'
]

(** [board_test] is the collection of tests testing Board's functions *)
let board_test = 
  Board.print_board empty_board;
  Board.print_board half_board;
  Board.print_board full_board;
  [
  board_is_empty_test "Pos 0 0 on an empty board should be empty" empty_board
    0 0 true;
  board_is_empty_test "Pos 0 0 of full_board is not empty" full_board 0 0 false;
  board_is_empty_test "Pos 14 14 of half_board is empty" half_board 14 14 true;
  board_is_empty_test "Pos 14 14 of full board is not empty" 
    full_board 14 14 false;
  board_get_char_raises_test "Pos 0 0 of empty_board raises 'EmptySpace'" 
    empty_board 0 0 (Board.EmptySpace);
  board_get_char_test "Pos 0 0 of half board is 'A'" half_board 0 0 'A';
  board_get_char_test "Pos 0 14 of half_board is 'O'" half_board 0 14 'O';
  board_get_char_raises_test "Pos 10 9 of half_board raises 'EmptySpace'" 
    half_board 10 9 Board.EmptySpace;
  board_get_char_test "Pos 10 9 of full_board is 'S'" full_board 10 9 'D';
  board_get_char_test "Pos 14 14 of full_board is 'Q'" full_board 14 14 'Q';
  board_set_char_test "Set the top left corner of empty_board to be 'A'" 
    empty_board 0 0 'A';
  board_set_char_test "Set Pos 6 6 of empty_board to character 'Z'"
    empty_board 6 6 'Z';
  board_set_char_test "Set pos 14 14 of half_board to 'F'" half_board 14 14 'F';
  board_set_char_raises_test "Set pos 6 6 of half_board to character 'Z'"
    half_board 6 6 'Z' Board.PosOccupied;
  board_set_char_raises_test "Set pos -1 100 of empty board raises UnknownPos"
    empty_board (-1) 100 'A' Board.UnknownPos;
  board_set_char_raises_test "Set pos 0 0 of empty_board to '@' raises 
    CharacterNotInAlphabet" empty_board 0 0 '@' Board.CharacterNotInAlphabet;

]

(***)
let bag_helper_test name bag c f expected_output =
  name >:: fun _ -> assert_equal expected_output (f bag c)
  ~printer:string_of_int

let bag_helper_test2 name bag f expected_output =
  name >:: fun _ -> assert_equal expected_output (f bag)
  ~printer:string_of_int
let init = init_bag

let update_bag = next_tile init
(* let update_2 = next_tile update_bag *)
let bag_test = [
  (* bag_helper_test "Asserting value of A is correct" init 'A' tile_value 1;
  bag_helper_test "Asserting value of Z is correct" init 'Z' tile_value 10; 
  bag_helper_test "Asserting initial count of Z is correct" init 'Z' tile_count 
  1;
  bag_helper_test "Asserting initial count of 0 is correct" init 'O' tile_count 
  8; 
  bag_helper_test2 "Asserting total tiles is updated" update_bag total_count
  101; 
  bag_helper_test2 "Asserting total tiles is updated after 2 tiles drawn" 
  update_2 total_count 100;
  bag_helper_test "Tiles Count is properly updated for O" update_bag 'O' tile_count 
  7; *)

]

 let generate_hand_test name hand expected_output =
  name >:: fun _ -> assert_equal expected_output (List.length (Hand.generate_hand hand))
  
let hand_test = [

  generate_hand_test "Test size of Hand" [] 7;

]
(** Test suite of all scrabble test*)
let suite =
  "test suite of all Scrabble Tests"
  >::: List.flatten [ board_test; hand_test]

(** Runs the test suite on run *)
let _ = run_test_tt_main suite