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
    Board.set_char board row col new_char;
    assert_equal new_char (Board.get_char board row col) 
      ~printer:Char.escaped

(** [board_set_char_raises_test name board row col chr expected_error] is an
    oUnit test named [name] which checks that the error raised by 
    [Board.set_char board row col chr] is [expected_error] *)
let board_set_char_raises_test name board row col chr expected_error =
  name >:: fun _ -> 
    assert_raises expected_error (fun _ -> Board.set_char board row col chr)

(** [empty_board] is an empty board. *)
let empty_board : Board.t = Array.make_matrix 15 15 Empty

(** [board_builder_helper current_board counter chr_lst] iterates through each 
    character in [chr_lst], and puts it in the next available position of 
    [current_board] *)
let rec board_builder_helper current_board counter chr_lst = match chr_lst with
| [] -> current_board
| h :: t ->
  Board.set_char current_board (counter / 15) (counter mod 15) h;
  board_builder_helper current_board (counter + 1) t

(** [board_builder chr_lst] fills a board with the letters from [chr_lst], 
    starting at the top row, and going left to right. When one row gets filled, 
    the next row begins being filled, and so on. 
    Raises: PosUnknown if [chr_lst] is longer than 225 characters *)
let board_builder chr_lst = 
  board_builder_helper (Array.make_matrix 15 15 Empty) 0 chr_lst

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

(** [real_board] is a board that actually contains words.*)
let real_board : Board.t = Array.make_matrix 15 15 Board.Empty

(**[real_board_init] initializes [real_board] with the word 
    "CAR" in the middle *)
let real_board_init = 
  set_char real_board 6 7 'C';
  set_char real_board 7 7 'A';
  set_char real_board 8 7 'R'

(**[string_of_int_option num] returns a string based on the int option [num]. *)
let string_of_int_option = function
| None -> "None"
| Some(num) -> "Some " ^ (Int.to_string num)

(** [board_check_word_test name board start_row start_col end_row end_col 
    expected] is an oUnit test which checks that the result from 
    [Board.check_word board start_row start_col end_row end_col] is expected.
    [steps] is a function of type unit, which will run before check_word runs *)
let board_check_word_test name board start_row start_col end_row end_col 
  (steps : 'a -> unit) expected =
  name >:: fun _ ->
    steps ();
    let actual = Board.check_word board start_row start_col end_row end_col in
    assert_equal expected actual ~printer:string_of_int_option

(** [error_notif] is a function which prints out a notification to the console,
  telling the tester that the error print out are not a failure of the function, 
  but rather meant to be informative. of why a word was rejected. *)
let error_notif _ = 
  print_newline ();
  print_string "**************************************************************";
  print_newline();
  print_string " If a printout appears below, saying 
  \"Word rejected due to ...\", this is expected. Whenever Board.check_word is 
  called on an invalid word, the error is printed out to console, so the player 
  knows why their word was rejected.";
  print_newline();
  print_newline ();
  print_string "  This is NOT a failure raised by oUnit. This error simply means
  the word was rejected, and tis to be expected if you are testing 
  Board.check_word on invalid words.";
  print_newline();
  print_string "**************************************************************";
  print_newline()

(** [cat _] are the functions required to change real_board, so that the word
    CAT is placed. *)
let cat _ = 
  set_char real_board 7 6 'C';
  set_char real_board 7 8 'T'

(** [ice _ ] are the functions requires to change real_board so that ICE is 
    placed.*)
let ice _ =
  set_char real_board 6 6 'I';
  set_char real_board 6 8 'E'

(** [zicu _] are the functions required to place ZICU (a gibberish word) on
  the board.*)
let zicu _ =
  set_char real_board 5 6 'Z';
  set_char real_board 8 6 'U'

let floating_char () = 
  set_char real_board 0 0 'C';
  set_char real_board 0 1 'H';
  set_char real_board 0 2 'A';
  set_char real_board 0 3 'R'

let floating_with_real_word () =
  floating_char ();
  set_char real_board 6 5 'L'

(** [board_test] is the collection of tests testing Board's functions *)
let board_test = 
  Board.print_board empty_board;
  Board.print_board half_board;
  Board.print_board full_board;
  Board.print_board real_board;
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
  board_check_word_test "Check word CAR in real_board should return Some 5" 
    real_board 6 7 8 7 (fun _ -> ()) (Some (5));
  board_check_word_test "Checking CAR again in the same place should return
    Some 0, because CAR wasn't modified" 
    real_board 6 7 8 7 (fun _ -> ()) (Some (0)); 
  board_check_word_test "Check word CAT, using the A in CAR, will only return
   5 points, because CAR remains unmodified" 
   real_board 7 6 7 8 cat (Some (5));
  board_check_word_test "Checking ICE will return Some 11" 
    real_board 6 6 6 8 ice (Some (11));
  board_check_word_test "Checking a word at an index out of range of 0..14 
    will return None" real_board (-1) 7 6 7 error_notif None;
  board_check_word_test "Checking a word that has an end position higher up than
    it's start position returns None" real_board 10 10 7 10 (fun _ -> ()) None;
  board_check_word_test "Checking a word that has an end position further left 
    than its start position returns None" real_board 7 7 7 3 (fun _ -> ()) None;
  board_check_word_test "Checking a word that's start and end positions are 
    diagonal to each other returns None" real_board 2 2 5 5 (fun _ -> ()) None;
  board_check_word_test "Checking a word on a board without a tile in the center 
    returns None" half_board 0 0 0 6 (fun _ -> ()) None;
  board_check_word_test "Checking a word with a space in it returns None"
    real_board 7 6 7 9 (fun _ -> ()) None;
  board_check_word_test "Checking a nonreal word returns None" 
    real_board 5 6 8 6 zicu None;
  board_check_word_test "Checking a floating word returns None" 
    real_board 0 0 0 3 floating_char None;
  board_check_word_test "Checking a real word, while there's floating characters
   elsehwere" real_board 6 5 6 8 floating_with_real_word None;
]

(***)
let bag_helper_test name c f expected_output =
  name >:: fun _ -> assert_equal expected_output (f c)
  ~printer:string_of_int

let bag_helper_test2 name bag f expected_output =
  name >:: fun _ -> assert_equal expected_output (f bag)
  ~printer:string_of_int

let next_tile_exception_helper name bag expected_output : test =
    name >:: fun _ -> assert_raises expected_output (fun () ->next_tile bag)
  
let tile_value_exception_helper name c expected_output : test =
      name >:: fun _ -> assert_raises expected_output (fun () -> tile_value c)
let init = init_bag
let update_bag = fst(next_tile init)
let update_2 = fst(next_tile update_bag)
let third_drawn = snd (next_tile update_2)

let u = return_tile third_drawn update_2

let empt = empty_bag
let bag_test = [
  bag_helper_test "Asserting value of A is correct" 'A' tile_value 1;
  bag_helper_test "Asserting value of Z is correct" 'Z' tile_value 10; 
  bag_helper_test "Asserting blank tiles have correct value" ' ' tile_value 0; 
  bag_helper_test2 "Asserting total tiles is updated after 2 draws" update_2 
  total_count 98; 
  bag_helper_test2 "Asserting total tiles is updated after third tile is drawn 
  and then replaced" update_2 total_count 98; 
  next_tile_exception_helper "Asserting tiles can't be drawn from empty bag" 
  empt EmptyBag;
  tile_value_exception_helper "Asserting that InvalidChar exception is raised 
  when the character was not in the initial bag" 'a' InvalidChar;
  
]

let create_starting_hand_test name hand bag expected_output =
  name >:: fun _ -> assert_equal expected_output (create_starting_hand hand bag)

let print_hor_test name hand expected_output =
  name >:: fun _ -> assert_equal expected_output (print_hor hand)

let print_vert_test name hand expected_output =
  name >:: fun _ -> assert_equal expected_output (print_vert hand)

let tile_replace_test name tile hand bag expected_output =
  name >:: fun _ -> assert_equal expected_output (tile_replace tile hand bag)

let tile_replace_exception_test name tile hand bag expected_output =
  name >:: fun _ -> 
    assert_raises expected_output (fun () -> tile_replace tile hand bag)

let place_a_letter_test name board letter pos hand expected_output =
  name >:: fun _ -> assert_equal expected_output (place_a_letter board letter pos hand)

let place_a_letter_exception_test name board letter pos hand expected_output =
  name >:: fun _ -> assert_raises expected_output (fun () -> place_a_letter board letter pos hand)

let tile_getter_test name letter hand ind expected_output =
  name >:: fun _ -> assert_equal expected_output (tile_getter letter hand ind)

let tile_getter_exception_helper name letter hand ind expected_output : test =
    name >:: fun _ -> assert_raises expected_output (fun () ->tile_getter letter
    hand ind)
let test_hand = Array.make 7 {letter = 'A'; value = 1 }

let test_hand_dup = Array.make 7 {letter = 'A'; value = 1 }
let test_hand2 = Array.append (Array.make 6 {letter = 'A'; value = 1 } ) 
(Array.make 1{letter='B'; value = 3})

let test_hand3 = Array.append (Array.make 6 {letter = 'A'; value = 1 } ) 
(Array.make 1{letter='*'; value = 0})

let hand_test = [
  create_starting_hand_test "Creating a starting hand raises no exceptions" 
  test_hand_dup init ();
  print_hor_test "Printing a starting hand raises no exceptions"
  test_hand (); 
  print_vert_test "Printing a starting hand raises no exceptions"
  test_hand (); 
  tile_replace_test "Replacing a tile in the hand returns unit" 
  {letter = 'A'; value = 1} test_hand init ();
  tile_replace_test "Replacing an empty space in the hand returns unit" 
  {letter = '*'; value = 0} test_hand init ();

  tile_replace_exception_test 
  "Replacing a tile that isn't in the hand returns unit" 
  {letter = 'B'; value = 3} test_hand init TileNotFound;

  place_a_letter_test "Place a legal letter" real_board "A" "0 0" 
  (Array.to_list test_hand) ();

  place_a_letter_exception_test 
  "Placing a letter not in the hand raises LetterNotFound" 
  real_board "B" "0 0" (Array.to_list test_hand) LetterNotFound;

  place_a_letter_exception_test 
  "Placing a letter into an position not on the board raises UnkownPos" 
  real_board "A" "-1 -1" (Array.to_list test_hand) UnknownPos;

  place_a_letter_exception_test 
  "Placing a letter into an occupied position raises PosOccupied" 
  real_board "A" "6 7" (Array.to_list test_hand) PosOccupied;

  (* tile_getter_test "Asserting tile in hand is retrievable" 'A' test_hand 0 
  {letter = 'A'; value = 10 }; *)
  tile_getter_test "Checking recursive call properly works when tile needed is 
  not first one" 'B' test_hand2 0 {letter = 'B'; value = 3 };
  tile_getter_exception_helper "Checking exception is raised when tile is not in
  hand" 'C' test_hand 0 LetterNotFound;
  tile_getter_exception_helper "Checking exception is raised when index is g
  greater than length of hand" 'C' test_hand 8 LetterNotFound;
]
(** Test suite of all scrabble test*)
let suite =
  "test suite of all Scrabble Tests"
  >::: List.flatten [ board_test;bag_test; hand_test ]

(** Runs the test suite on run *)
let _ = run_test_tt_main suite