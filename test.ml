(**TEST PLAN: The scrabble game mainly comprised of three backend modules:
bag, board, and hand. As all three components of the scrabble game involved 
mutable structures, it was harder to test some of the aspects especially when 
the structure was just being mutated. There were also certain functions that 
required user input, making them hard to be tested. For function that had a 
non-unit output, white box testing was used. We looked at the code written and 
tried to develop test cases that would catch errors and the various cases 
that could occur because of conditionals. For testing the functions that had
unit as an output, the function was first called. Then, we would test aspects 
of the system that we could check (i.e. that the total number of tiles in bag 
was updated and correct, the hand does not contain a certain letter anymore, 
etc.). So, we manually tested these. We are confident our system is correct 
because all of the exposed functions were tested in some way, either 
automatically or manually, and our main module containing the front end of the 
scrabble game used all of these functions.*)


open OUnit2
open Board
open Bag
open Hand

let j = Yojson.Basic.from_file "bag.json"
let j1 = Yojson.Basic.from_file "empty.json"
(** [board_is_empty_test name board row col] is an oUnit test named [name] which
    checks that [Board.is_empty board row col] is [expecetd_bool] *)
let board_is_empty_test name board row col expected_bool =
  name >:: fun _ -> assert_equal expected_bool (Board.is_empty board row col) 
    ~printer:Bool.to_string

let board_is_empty_raises_test name board row col error =
  name >:: fun _ -> assert_raises error (fun () -> Board.is_empty board row col)

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
let empty_board : Board.t = board_init ()

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
  board_builder_helper (board_init ()) 0 chr_lst

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
let real_board : Board.t = board_init ()

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

let not_in_center () =
  set_char empty_board 8 6 'B';
  set_char empty_board 8 7 'O';
  set_char empty_board 8 8 'B'

let one_character_in_center () =
  set_char empty_board 7 7 'A';
  not_in_center ()

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

let floating_bat () =
  set_char real_board 0 14 'B';
  set_char real_board 1 14 'A';
  set_char real_board 2 14 'T'

let words_in_two_directions () =
  set_char real_board 7 3 'T';
  set_char real_board 7 4 'O';
  set_char real_board 7 5 'M';
  set_char real_board 8 3 'R';
  set_char real_board 9 3 'I';
  set_char real_board 10 3 'C';
  set_char real_board 11 3 'K'

let word_with_space () =
  set_char real_board 7 2 'S';
  set_char real_board 7 3 'E';
  set_char real_board 7 4 'A'

let vert_word_with_space () =
  set_char real_board 1 7 'G';
  set_char real_board 2 7 'R';
  set_char real_board 3 7 'I';
  set_char real_board 4 7 'P'

let long_made_up_word1 () = 
  set_char real_board 5 0 'A';
  set_char real_board 5 1 'P';
  set_char real_board 5 2 'Q';
  set_char real_board 5 3 'D';
  set_char real_board 5 4 'R';
  set_char real_board 5 5 'F';
  set_char real_board 5 6 'G';
  set_char real_board 5 7 'H';
  set_char real_board 5 8 'S';
  set_char real_board 5 9 'J';
  set_char real_board 5 10 'K';
  set_char real_board 5 11 'L';
  set_char real_board 5 12 'M';
  set_char real_board 5 13 'N';
  set_char real_board 5 14 'O'

  let long_made_up_word2 () = 
    set_char real_board 5 0 'T';
    set_char real_board 5 1 'V';
    set_char real_board 5 2 'Q';
    set_char real_board 5 3 'D';
    set_char real_board 5 4 'R';
    set_char real_board 5 5 'W';
    set_char real_board 5 6 'G';
    set_char real_board 5 7 'H';
    set_char real_board 5 8 'S';
    set_char real_board 5 9 'X';
    set_char real_board 5 10 'K';
    set_char real_board 5 11 'L';
    set_char real_board 5 12 'Y';
    set_char real_board 5 13 'N';
    set_char real_board 5 14 'O'
  
let tomcat () =
  set_char real_board 7 3 'T';
  set_char real_board 7 4 'O';
  set_char real_board 7 5 'M'

let atlas () =
  set_char real_board 6 3 'A';
  set_char real_board 8 3 'L';
  set_char real_board 9 3 'A';
  set_char real_board 10 3 'S'

let mesa () =
  set_char real_board 9 0 'M';
  set_char real_board 9 1 'E';
  set_char real_board 9 2 'S'

let myrica () =
  set_char real_board 10 0 'Y';
  set_char real_board 11 0 'R';
  set_char real_board 12 0 'I';
  set_char real_board 13 0 'C';
  set_char real_board 14 0 'A'

let raw () =
  set_char real_board 11 1 'A';
  set_char real_board 11 2 'W'

let acclimatization () =
  set_char real_board 14 1 'C';
  set_char real_board 14 2 'C';
  set_char real_board 14 3 'L';
  set_char real_board 14 4 'I';
  set_char real_board 14 5 'M';
  set_char real_board 14 6 'A';
  set_char real_board 14 7 'T';
  set_char real_board 14 8 'I';
  set_char real_board 14 9 'Z';
  set_char real_board 14 10 'A';
  set_char real_board 14 11 'T';
  set_char real_board 14 12 'I';
  set_char real_board 14 13 'O';
  set_char real_board 14 14 'N'

let vert_acclimatization () =
  set_char real_board 0 14 'A';
  set_char real_board 1 14 'C';
  set_char real_board 2 14 'C';
  set_char real_board 3 14 'L';
  set_char real_board 4 14 'I';
  set_char real_board 5 14 'M';
  set_char real_board 6 14 'A';
  set_char real_board 7 14 'T';
  set_char real_board 8 14 'I';
  set_char real_board 9 14 'Z';
  set_char real_board 10 14 'A';
  set_char real_board 11 14 'T';
  set_char real_board 12 14 'I';
  set_char real_board 13 14 'O'

let pleuropneumonia () =
  set_char real_board 0 0 'P';
  set_char real_board 0 1 'L';
  set_char real_board 0 2 'E';
  set_char real_board 0 3 'U';
  set_char real_board 0 4 'R';
  set_char real_board 0 5 'O';
  set_char real_board 0 6 'P';
  set_char real_board 0 7 'N';
  set_char real_board 0 8 'E';
  set_char real_board 0 9 'U';
  set_char real_board 0 10 'M';
  set_char real_board 0 11 'O';
  set_char real_board 0 12 'N';
  set_char real_board 0 13 'I'

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
  board_is_empty_raises_test "is_empty on -1 1 raises UnknownPos" 
    empty_board (-1) 1 Board.UnknownPos;
  board_is_empty_raises_test "is_empty on 1 15 raises UnknownPos"
    empty_board 1 15 Board.UnknownPos;
  board_is_empty_raises_test "is_empty on 15 1 raises UnknownPos"
    empty_board 15 1 Board.UnknownPos;
  board_is_empty_raises_test "is_empty on 14 -1 raises UnknownPos"
    empty_board 14 (-1) Board.UnknownPos;
  board_get_char_raises_test "Pos 0 0 of empty_board raises 'EmptySpace'" 
    empty_board 0 0 (Board.EmptySpace);
  board_get_char_test "Pos 0 0 of half board is 'A'" half_board 0 0 'A';
  board_get_char_test "Pos 0 14 of half_board is 'O'" half_board 0 14 'O';
  board_get_char_raises_test "Pos 10 9 of half_board raises 'EmptySpace'" 
    half_board 10 9 Board.EmptySpace;
  board_get_char_raises_test "get_char on -1 1 raises UnknownPos" 
    empty_board (-1) 1 Board.UnknownPos;
  board_get_char_raises_test "get_char on 1 15 raises UnknownPos"
    empty_board 1 15 Board.UnknownPos;
  board_get_char_raises_test "get_char on 15 1 raises UnknownPos"
    empty_board 15 1 Board.UnknownPos;
  board_get_char_raises_test "get_char on 14 -1 raises UnknownPos"
    empty_board 14 (-1) Board.UnknownPos;
  board_get_char_test "Pos 10 9 of full_board is 'S'" full_board 10 9 'D';
  board_get_char_test "Pos 14 14 of full_board is 'Q'" full_board 14 14 'Q';
  board_set_char_test "Set the top left corner of empty_board to be 'A'" 
    empty_board 0 0 'A';
  board_set_char_test "Set Pos 6 6 of empty_board to character 'Z'"
    empty_board 6 6 'Z';
  board_set_char_test "Set pos 14 14 of half_board to 'F'" half_board 14 14 'F';
  board_set_char_raises_test "Set pos 6 6 of half_board to character 'Z'"
    half_board 6 6 'Z' Board.PosOccupied;
  board_set_char_raises_test "set_char on -1 1 raises UnknownPos" 
    empty_board (-1) 1 'A' Board.UnknownPos;
  board_set_char_raises_test "set_char on 1 15 raises UnknownPos"
    empty_board 1 15 'A' Board.UnknownPos;
  board_set_char_raises_test "set_char on 15 1 raises UnknownPos"
    empty_board 15 1 'A' Board.UnknownPos;
  board_set_char_raises_test "set_char on 14 -1 raises UnknownPos"
    empty_board 14 (-1) 'A' Board.UnknownPos;
  board_set_char_raises_test "set_char with ~ raises CharacterNotInAlphabet"
    empty_board 8 8 '~' Board.CharacterNotInAlphabet;
  board_set_char_raises_test "set_char using a number raises 
    CharacterNotInAlphabet" empty_board 9 9 '2' Board.CharacterNotInAlphabet;
  board_set_char_raises_test "Set pos -1 100 of empty board raises UnknownPos"
    empty_board (-1) 100 'A' Board.UnknownPos;
  board_set_char_raises_test "Set pos 0 0 of empty_board to '@' raises 
    CharacterNotInAlphabet" empty_board 0 0 '@' Board.CharacterNotInAlphabet;
  board_check_word_test "Place on character in the center, then the rest 
    elsewhere" empty_board 8 5 8 7 one_character_in_center None;
  board_check_word_test "checking the first word, when it's not in the center 
    returns None" empty_board 8 6 8 8 not_in_center None;
  board_check_word_test "Checknig a one letter word returns None" 
    empty_board 7 7 7 7 (fun () -> set_char empty_board 7 7 'I') None;
  board_check_word_test "Check word CAR in real_board should return Some 10" 
    real_board 6 7 8 7 (fun _ -> ()) (Some (10));
  board_check_word_test "Checking CAR again in the same place should return
    Some 0, because CAR wasn't modified" 
    real_board 6 7 8 7 (fun _ -> ()) (Some (0)); 
  board_check_word_test "Check word CAT, using the A in CAR, will only return
   5 points, because CAR remains unmodified" 
   real_board 7 6 7 8 cat (Some (5));
  board_check_word_test "Checking ICE will return Some 11" 
    real_board 6 6 6 8 ice (Some (15));
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
  board_check_word_test "Checking a vertical floating word returns none"
    real_board 0 14 2 14 floating_bat None;
  board_check_word_test "Checking a real word, while there's floating characters
   elsehwere" real_board 6 5 6 8 floating_with_real_word None;
  board_check_word_test "Checking a word, when the user places tiles in two 
   different directions, returns None" real_board 7 3 7 8 
   words_in_two_directions None;
  board_check_word_test "Checking a word with a space in it returns none" 
    real_board 7 3 7 8 word_with_space None;
  board_check_word_test "checking a vertical word with a space returns none"
    real_board 1 7 8 7 vert_word_with_space None;
  board_check_word_test "Checking bcat returns none" real_board 7 5 7 8 
    (fun () -> set_char real_board 7 5 'B') None;
  board_check_word_test "Check_word on a long made-up word" real_board 5 0 5 14
    long_made_up_word1 None;
  board_check_word_test "Check_word on another long made-up word" 
    real_board 5 0 5 14 long_made_up_word2 None;
  board_check_word_test "Check tomcat returns 11" real_board 7 3 7 8
    tomcat (Some(11));
  board_check_word_test "Check atlas returns 4" real_board 6 3 10 3 
    atlas (Some(5));
  board_check_word_test "Check mesa returns 8" real_board 9 0 9 3
    mesa (Some(8));
  board_check_word_test "Check myrica returns 42" real_board 9 0 14 0
    myrica (Some(42));
  board_check_word_test "Check raw returns 6" real_board 11 0 11 2
    raw (Some(6));
  board_check_word_test "Check acclimatization (two 3W tiles) returns 288" 
    real_board 14 0 14 14 acclimatization (Some(288));
  board_check_word_test "Check acclimatization, but this time along the 
    right vertical edge" real_board 0 14 14 14 vert_acclimatization (Some(288));
  board_check_word_test "Check pleuropneumonia across the top edge"
    real_board 0 0 0 14 pleuropneumonia (Some(207));

  board_check_word_test "This is just here to print the board when you're done"
    real_board 0 0 0 0 (fun () -> print_board real_board) None;
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
let init = bag_of_json j 
let update_bag = fst(next_tile init)
let update_2 = fst(next_tile update_bag)
let third_drawn = snd (next_tile update_2)

let empt = bag_of_json j1

let replaced_tiles_hand = Array.make 7 {letter = '*'; value = 0 } 

let new_in = bag_of_json j
let bag_test = 
  return_tile third_drawn update_2;
  tile_replace {letter = '*'; value = 0 } replaced_tiles_hand new_in ;
  [
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
  bag_helper_test2 "Asserting total tiles is updated after replacing an entire 
  hand" new_in total_count 93;
  
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
  name >:: fun _ -> assert_equal expected_output 
  (place_a_letter board letter pos hand)

let place_a_letter_exception_test name board letter pos hand expected_output =
  name >:: fun _ -> assert_raises expected_output (fun () -> 
    place_a_letter board letter pos hand)

let tile_getter_test name letter hand ind expected_output =
  name >:: fun _ -> assert_equal expected_output (tile_getter letter hand ind)

let tile_getter_exception_helper name letter hand ind expected_output : test =
    name >:: fun _ -> assert_raises expected_output (fun () ->tile_getter letter
    hand ind)
let test_hand = Array.make 7 {letter = 'A'; value = 1 }

let test_hand_dup = Array.make 7 {letter = 'A'; value = 1 }

let test_hand_dup2 = Array.make 7 {letter = 'A'; value = 1 }
let test_hand2 = Array.append (Array.make 6 {letter = 'A'; value = 1 } ) 
(Array.make 1{letter='B'; value = 3})

let test_hand3 = Array.append (Array.make 6 {letter = 'A'; value = 1 } ) 
(Array.make 1{letter='*'; value = 0})

let placed_hand = Array.make 7 {letter = '*'; value = 0 }

let placed_tile_list = [{letter = 'A'; value = 1 }; {letter = 'A'; value = 1 };
{letter = 'A'; value = 1 };{letter = 'A'; value = 1 };
{letter = 'A'; value = 1 };{letter = 'A'; value = 1 };
{letter = 'A'; value = 1 }]

let hand_test = 
  revert_hand placed_hand placed_tile_list;
  [
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
  "Replacing a tile that isn't in the hand raises TileNotFound" 
  {letter = 'B'; value = 3} test_hand_dup2 init TileNotFound;

  place_a_letter_test "Place a legal letter" empty_board "A" "0 0" 
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
  hand" 'C' test_hand_dup2 0 LetterNotFound;
  tile_getter_exception_helper "Checking exception is raised when index is g
  greater than length of hand" 'C' test_hand_dup 8 LetterNotFound;
  tile_getter_exception_helper "Making sure that every intermediate * tile in 
  reverted back to the original hand after the failure of a word" '*' 
  placed_hand 0 LetterNotFound;
]
(** Test suite of all scrabble test*)
let suite =
  "test suite of all Scrabble Tests"
  >::: List.flatten [ board_test;bag_test; hand_test ]

(** Runs the test suite on run *)
let _ = run_test_tt_main suite