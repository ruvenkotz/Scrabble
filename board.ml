open Yojson.Basic.Util

type mult =
| Letter of int
| Whole of int

(** [space] represents the value contained at a location. It is either Empty, or
  a Char *)
type space =
| Empty
| Multiplier of mult
| Char of char

exception UnknownPos

exception PosOccupied

exception EmptySpace

exception CharacterNotInAlphabet

exception FloatingLetter

(** Raised when checking if a word is valid, but no tile is in the center. 
  Scrabble rules say that the first word must place a tile in the center, 
  So if no tile is in the cneter, that rule has been broken. *)
exception NoTileInCenter

(** Raised when attempting to check a word, but the start position is further 
    up or left of the end position, or if the start and end position are 
    diagonal from each other. *)
exception InvalidStartOrEndPos

(** Raised when checking a word, but that word is not in the 
    English dicitonary. *)
exception NonRealWord

(** Raised when attemptng to access an index of an array outside of the array's 
    size*)
exception IndexOutOfBounds

(** [row] is an array a spaces, organized from left to right. 
Example: The row:
          A B C D E F 
          Would be represented as:
          ['A';'B';'C';'D';'E';'F']          
Requires: Must be 15 spaces long. *)
type row = space array

let bag = Bag.init_bag

(** [t] represents the board itself, and it consists of an array of rows. 
The first row represents the topmost row, the second row represents the second 
row from the top, etc...
Example: The board:
          A B C D E F
          G H I J K L
          M N O P Q R
          S T U V W X
          Would be represented as:
          [['A';'B';'C';'D';'E';'F'];['G';'H';'I';'J';'K';'L'];
          ['M';'N';'O';'P';'Q';'R'];['S';'T';'U';'V';'W';'X']] 
Requires: must be 15 rows long *)
type t = row array

(** [multiplier_json] is the json object containing all the mulipliers of the 
    board.*)
let multiplier_json = Yojson.Basic.from_file "multiplier.json"

type mult_and_pos =
| Mult_and_pos of int * int * mult

(** [set_multiplier board multiplier] sets the multiplier of a certain 
    location on the board to the value specified. [Multiplier] is of type 
    [mult_and_pos], where the position and value is stored as (row, col, mult),
    where [mult] is of type [mult], and tells whether it's a letter multiplier 
    or whole word multiplier, and it's multiplying factor. *)
let set_multiplier (board : t) = function
| Mult_and_pos (row, col, mult) -> board.(row).(col) <- Multiplier mult


(**[mult_and_pos_of_json json_obj] returns the multiplier and position from a 
    json record *)
let mult_and_pos_of_json json_obj =
  let row = json_obj |> member "row" |> to_int
  and col = json_obj |> member "col" |> to_int
  and letter = json_obj |> member "letter" |> to_bool
  and value = json_obj |> member "value" |> to_int in
  if letter then Mult_and_pos (row, col, Letter(value)) else
    Mult_and_pos (row, col, Whole(value))

let board_init () : t =
  let board = Array.make_matrix 15 15 Empty in
  multiplier_json |> member "multipliers" |> to_list |> 
  List.map mult_and_pos_of_json |> List.iter (set_multiplier board);
  board
  

(** [char_of_space spce] converts an object of type space into a character. 
    Raises EmptySpace if [spce] is Empty. *)
let char_of_space (spce : space) : char = match spce with
| Empty | Multiplier _ -> raise(EmptySpace)
| Char(chr) -> chr

let is_empty board row col =
  if row > 14 || row < 0 || col > 14 || col < 0 then raise(UnknownPos) else
    let current_space = board.(row).(col) in
    match current_space with
    | Empty | Multiplier _ -> true
    | Char(_) -> false

let get_char (board : t) (row : int) (col : int) : char =
  if row > 14 || row < 0 || col > 14 || col < 0 then raise(UnknownPos) else
    let current_row = board.(row) in
    current_row.(col) |> char_of_space

let set_char (board : t) (row : int) (col : int) (chr : char) : unit = 
  if row > 14 || row < 0 || col > 14 || col < 0 then raise(UnknownPos) else
    let capital_chr = Char.uppercase_ascii chr in
    if Char.code capital_chr < 65 || Char.code capital_chr > 90 then 
      raise(CharacterNotInAlphabet) else
    if is_empty board row col then board.(row).(col) <- Char(capital_chr) else
      raise(PosOccupied)

let print_board (board : t) : unit = 
  let print_space = function 
  | Empty -> print_string (" ** "); 
  | Multiplier m -> begin
    match m with
    | Letter v -> print_string (" " ^ string_of_int v ^ "L ")
    | Whole v -> print_string (" " ^ string_of_int v ^ "W ") end
  | Char(chr) -> print_string (" " ^ Char.escaped chr ^ "  "); in
  let print_row row = Array.iter print_space row; print_newline(); in
  Array.iter print_row board; print_newline()

(** [string_of_space space] returns the string represenation of [space]. 
  Empty spaces are represented by '*' and characters are represented as 
  themselves.*)
  let string_of_space = function
  | Empty | Multiplier _ -> "*"
  | Char c -> Char.escaped c
  
(** [space_string_combiner space str] concatenates the string represenation of 
    [space] with the string [str ]*)
let space_string_combiner space str = string_of_space space ^ str
  
(** [string_of_row row] converts [row] into its string representation. *)
let string_of_row row = Array.fold_right space_string_combiner row " "
  
(** [row_string_combiner row str] concatenates the string representation of 
    [row] with the string [str]*)
let row_string_combiner row str = string_of_row row ^ str
  
let string_of_board board = Array.fold_right row_string_combiner board ""

(** dir is a private type, used to store whether the word is vertical or 
    horizontal, and how long the word is*)
type dir =
| Vert of int
| Hor of int

(** [length_and_dir start_row start_col end_row end_col] returns an object of 
    type dir, along with the length of the word. This raises an assertionError
    if the start or end position is not within 0 or 14, and raises 
    InvalidStartOrEndPos if the start_position is further left or up of the 
    end position, or if the start and end positions are diagonal to each 
    other.*)
let length_and_dir start_row start_col end_row end_col =
  assert (end_col <= 14 && end_col >= 0);
  assert (start_col <= 14 && start_col >= 0);
  assert (end_row <= 14 && end_row >= 0);
  assert (start_row <= 14 && start_row >= 0);
  if (end_row - start_row = 0) then begin
    if end_col > start_col then Hor(end_col - start_col + 1) else 
      raise(InvalidStartOrEndPos) end else
  if (end_col - start_col = 0) then begin
    if end_row > start_row then Vert(end_row - start_row + 1) else 
      raise(InvalidStartOrEndPos) end else
  raise(InvalidStartOrEndPos)

(** [find_leftmost_nonempty_space board row col] returns the leftmost space 
    from (row, col) that's nonempty. *)
let rec find_leftmost_nonempty_space board row col = 
  if col = 0 || is_empty board row (col - 1) then (row, col) else
  find_leftmost_nonempty_space board row (col - 1)

(** [find_rightmost_nonempty_space board row col] returns the rightmost space 
  from (row, col) that's nonempty. *)
let rec find_rightmost_nonempty_space board row col = 
  if col = 14 || is_empty board row (col + 1) then (row, col) else
  find_rightmost_nonempty_space board row (col + 1)

(** [add_last_ver board start_row start_col acc] adds the two characters below 
    (start_row, start_col) to the string acc. *)
let add_last_ver board start_row start_col acc =
  let first_char = Char.escaped (get_char board start_row start_col) in
  let second_char = Char.escaped (get_char board (start_row + 1) start_col) in
  (acc ^ first_char) ^ second_char

(** [add_last_hor board start_row start_col acc] adds the two characters to
  the right (start_row, start_col) to the string acc. *)
let add_last_hor board start_row start_col acc =
  let first_char = Char.escaped (get_char board start_row start_col) in
  let second_char = Char.escaped (get_char board start_row (start_col + 1)) in
  (acc ^ first_char) ^ second_char

(** [build_word_helper board start_row start_col end_row end_col acc] 
    accumulates the word built so far in acc, and continues recursively until 
    the start position matches the end position. *)
let rec build_word_helper board start_row start_col end_row end_col acc =
  match length_and_dir start_row start_col end_row end_col with
  | Vert(len) -> if len = 2 then add_last_ver board start_row start_col acc else 
    let new_char = Char.escaped (get_char board start_row start_col) in
    let new_acc = acc ^ new_char in
    let new_row = start_row + 1 in
    build_word_helper board new_row start_col end_row end_col new_acc
  | Hor(len) -> if len = 2 then add_last_hor board start_row start_col acc else
    let new_char = Char.escaped (get_char board start_row start_col) in
    let new_acc = acc ^ new_char in
    let new_col = start_col + 1 in
    build_word_helper board start_row new_col end_row end_col new_acc

(** [build_word board start_row start_col end_row end_col] will return the
    string formed from characters between (start_row, start_col) and
    (end_row, end_col). 
    Raises EmptySpace if there's an empty space between (start_row, start_col)
      and (end_row, end_col)*)
let build_word board start_row start_col end_row end_col =
  build_word_helper board start_row start_col end_row end_col ""

(** [word] is a private type which is either NoWord, or a word, which consists 
    of the string itelf, the position where that word starts, and the direction 
    the word goes in. *)
type word =
| NoWord
| Word of string * (int * int) * dir

(** [current_words] is an array which maintains the current valid words on the 
    board, their positions, and their direction. *)
let current_words = Array.make 1000 NoWord

(** [possible_new_words] is an array which maintains all words formed by the 
    newly placed word. These are not guaranteed to be actual words, nor 
    guaranteed to have modified by the newly placed word. *)
let possible_new_words = Array.make 8 NoWord

(** [words_touching_but_not_modified] is an array which maintains the words 
    which are touched by the newly placed word, but not modified. In other 
    words, this turn's new word uses a tile from these words, without 
    changing these words.*)
let words_touching_but_not_modified = Array.make 20 NoWord

(** [find_next_index_helper arr acc] iterates over arr until an empty space is 
    found, and returns the accumulator [acc]*)
let rec find_next_index_helper arr acc = match arr.(acc) with
| NoWord -> acc
| Word(_) -> find_next_index_helper arr (acc + 1)

(** [find_next_index arr] finds the next empty index in array [arr]. *)
let find_next_index arr = find_next_index_helper arr 0

(** [place_word board word_obj] will place word [word] at its correpsonding 
  location on [board]. [word] is an object of type Word, which contains the 
  string representation of the word, it's starting location, and it's 
  direction and length*)
  let place_word board word_obj = match word_obj with
  | NoWord -> ()
  | Word(word, loc, dirct) -> 
    match dirct with
    | Vert(len) -> 
      for i=0 to len - 1 do
        let row = fst loc + i in
        let col = snd loc in
        if is_empty board row col then set_char board row col word.[i] else ()
      done
    | Hor(len) ->
      for i=0 to len - 1 do
        let row = fst loc in
        let col = snd loc + i in
        if is_empty board row col then set_char board row col word.[i] else ()
      done

(**[find_modified_words_vert board row col] finds all the horizontal 
    words which use the tile located at (row, col) *)
let find_modified_words_vert board prev_board row col : unit =
  let left = find_leftmost_nonempty_space prev_board row col in
  let right = find_rightmost_nonempty_space prev_board row col in
  if left = right then () else
    let new_word = 
      build_word board (fst left) (snd left) (fst right) (snd right) in
    let dir = length_and_dir (fst left) (snd left) (fst right) (snd right) in
    let word_and_loc  = Word(new_word, left, dir) in
    if Array.mem word_and_loc current_words then 
      let index = find_next_index words_touching_but_not_modified in
      words_touching_but_not_modified.(index) <- word_and_loc else
      let index = find_next_index possible_new_words in
      possible_new_words.(index) <- word_and_loc

(**[find_topmost_nonempty_space board row col] returns a tuple containing the 
    (row, col) of the topmost space that still contains a character starting 
    from (row, col) *)
let rec find_topmost_nonempty_space board row col = 
  if row = 0 || is_empty board (row - 1) col then (row, col) else
  find_topmost_nonempty_space board (row - 1) col

(** [find_bottommost_nonempty_space board row col] returns a tuple with the
     bottommost non empty space from the positoin row col. *)
let rec find_bottommost_nonempty_space board row col = 
  if row = 14 || is_empty board (row + 1) col then (row, col) else
  find_bottommost_nonempty_space board (row + 1) col

(** [find_modified_words_hor] finds all the vertical words which use the tile 
    located at (row, col). *)
let find_modified_words_hor board prev_board row col : unit =
  let top = find_topmost_nonempty_space prev_board row col in
  let bottom = find_bottommost_nonempty_space prev_board row col in
  if top = bottom then () else
    let new_word = 
      build_word board (fst top) (snd top) (fst bottom) (snd bottom) in
    let dir = length_and_dir (fst top) (snd top) (fst bottom) (snd bottom) in
    let word_and_loc  = Word(new_word, top, dir) in
    if Array.mem word_and_loc current_words then
      let index = find_next_index words_touching_but_not_modified in
      words_touching_but_not_modified.(index) <- word_and_loc else
    let index = find_next_index possible_new_words in
    possible_new_words.(index) <- word_and_loc

(** [index_of_helper] iterates over arr until obj is found, and returns the 
    current index [cur_index]*)
let rec index_of_helper arr obj cur_index =
  if arr.(cur_index) = obj then cur_index else
  index_of_helper arr obj (cur_index + 1)

(** [index_of arr obj] finds the index of obj in the array [arr] *)
let index_of arr obj = index_of_helper arr obj 0
  
(** [unique_words] is a function of type unit, which itertes over the array 
    [possible_new_words] and checks that all possible_new_words are 
    unique (I.E. not in [current_words].) If a word in [possible_new_words] is
    in [curent_words], then that word is removed from [possible_new_words]
    by setting the word in the space to NoWord.*)
let unique_words _ =
  let is_unique word = not (Array.mem word current_words) in
  let remove_if_not_modified word = 
    if is_unique word then () else
      let index = index_of possible_new_words word in
      possible_new_words.(index) <- NoWord in
  Array.iter remove_if_not_modified possible_new_words

(** [json] is the Yojson.Basic object read from the json file containing the 
    English dictionary. *)
let json = Yojson.Basic.from_file "words.json"

(**[dict] is a type to store the English dictionary, with separate lists 
    for each letter of the alphabet*)
type dict = {
  a : string list;
  b : string list;
  c : string list;
  d : string list;
  e : string list;
  f : string list;
  g : string list;
  h : string list;
  i : string list;
  j : string list;
  k : string list;
  l : string list;
  m : string list;
  n : string list;
  o : string list;
  p : string list;
  q : string list;
  r : string list;
  s : string list;
  t : string list;
  u : string list;
  v : string list;
  w : string list;
  x : string list;
  y : string list;
  z : string list;
}

(**[dictionary] is the record containing all words in the English language,
  separated by first letter. *)
let dictionary = {
  a = json |> member "A" |> to_list |> List.map to_string;
  b = json |> member "B" |> to_list |> List.map to_string;
  c = json |> member "C" |> to_list |> List.map to_string;
  d = json |> member "D" |> to_list |> List.map to_string;
  e = json |> member "E" |> to_list |> List.map to_string;
  f = json |> member "F" |> to_list |> List.map to_string;
  g = json |> member "G" |> to_list |> List.map to_string;
  h = json |> member "H" |> to_list |> List.map to_string;
  i = json |> member "I" |> to_list |> List.map to_string;
  j = json |> member "J" |> to_list |> List.map to_string;
  k = json |> member "K" |> to_list |> List.map to_string;
  l = json |> member "L" |> to_list |> List.map to_string;
  m = json |> member "M" |> to_list |> List.map to_string;
  n = json |> member "N" |> to_list |> List.map to_string;
  o = json |> member "O" |> to_list |> List.map to_string;
  p = json |> member "P" |> to_list |> List.map to_string;
  q = json |> member "Q" |> to_list |> List.map to_string;
  r = json |> member "R" |> to_list |> List.map to_string;
  s = json |> member "S" |> to_list |> List.map to_string;
  t = json |> member "T" |> to_list |> List.map to_string;
  u = json |> member "U" |> to_list |> List.map to_string;
  v = json |> member "V" |> to_list |> List.map to_string;
  w = json |> member "W" |> to_list |> List.map to_string;
  x = json |> member "X" |> to_list |> List.map to_string;
  y = json |> member "Y" |> to_list |> List.map to_string;
  z = json |> member "Z" |> to_list |> List.map to_string; 
}

(** [word_list] returns the list of words sarting with the letter [letter]. *)
let word_list letter =
  if letter = 'A' then dictionary.a else
  if letter = 'B' then dictionary.b else
  if letter = 'C' then dictionary.c else
  if letter = 'D' then dictionary.d else
  if letter = 'E' then dictionary.e else
  if letter = 'F' then dictionary.f else
  if letter = 'G' then dictionary.g else
  if letter = 'H' then dictionary.h else
  if letter = 'I' then dictionary.i else
  if letter = 'J' then dictionary.j else
  if letter = 'K' then dictionary.k else
  if letter = 'L' then dictionary.l else
  if letter = 'M' then dictionary.m else
  if letter = 'N' then dictionary.n else
  if letter = 'O' then dictionary.o else
  if letter = 'P' then dictionary.p else
  if letter = 'Q' then dictionary.q else
  if letter = 'R' then dictionary.r else
  if letter = 'S' then dictionary.s else
  if letter = 'T' then dictionary.t else
  if letter = 'U' then dictionary.u else
  if letter = 'V' then dictionary.v else
  if letter = 'W' then dictionary.w else
  if letter = 'X' then dictionary.x else
  if letter = 'Y' then dictionary.y else
  dictionary.z
  
(** [is_word_real word] checks if [word] is a word in the English dictionary. *)
let is_word_real = function
| NoWord -> true
| Word(word, _, _) -> 
  let first_letter = word.[0] in
  let lowercase_word = String.lowercase_ascii word in
  List.mem lowercase_word (word_list first_letter)

(** [are_words_real] is true if all the new and modified words of 
  [possible_new_words] are real words. *)
let are_words_real _ =
  unique_words ();
  Array.for_all is_word_real possible_new_words

(**[add_original_word board start_row start_col end_row end_col] adds the 
    original word to the array possible_new_words. *)
let add_original_word board start_row start_col end_row end_col =
  let index = find_next_index possible_new_words in
  let word = build_word board start_row start_col end_row end_col in
  let start_pos = (start_row, start_col) in
  let loc = length_and_dir start_row start_col end_row end_col in
  possible_new_words.(index) <- Word(word, start_pos, loc)

(** [muliplier_at_loc board loc] will return a pair containing the 
    (letter multiplier at [loc], whole word multiplier at [loc]) *)
 let multiplier_at_loc board loc =
  let row = fst loc
  and col = snd loc in
  match board.(row).(col) with
  | Empty | Char _ -> (1, 1)
  | Multiplier m -> 
    match m with
    | Letter v -> (v, 1)
    | Whole v -> (1, v)

(**[word_value_helper word acc] is the sum of letter values in the word so far. 
    Reminaing values to be counted are in string [word], and the accumulated 
    points so far are stored in int [acc].*)
let rec word_value_helper prev_board word loc dir acc =
  let multiplier = multiplier_at_loc prev_board loc in
  let letter_multiplier = fst multiplier in
  if String.length word = 0 then acc else
  let letter_val = (Bag.tile_value word.[0]) * letter_multiplier
  and substring_len = String.length word - 1 in
  let substring = String.sub word 1 substring_len in
  match dir with
  | Vert len -> 
    let new_loc = ((fst loc) + 1, snd loc)
    and new_dir = (Vert (len - 1))
    and new_acc = acc + letter_val in
    word_value_helper prev_board substring new_loc new_dir new_acc
  | Hor len -> 
    let new_loc = (fst loc, snd loc + 1)
    and new_dir = Hor (len - 1)
    and new_acc = acc + letter_val in
    word_value_helper prev_board substring new_loc new_dir new_acc

(**[word_multiplier_finder_helper prev_board loc dir acc] finds the multiplier 
    located at loc using prev_board, and multiplies it with the current
    multiplier for the rest word, [acc]*)
let rec word_multiplier_finder_helper prev_board loc dir acc =
  match dir with
  | Vert len ->
    if len > 0 then
      let new_loc = ((fst loc + 1), snd loc)
      and new_dir = (Vert (len - 1))
      and multiplier = snd (multiplier_at_loc prev_board loc) in
      let new_acc = acc * multiplier in
      word_multiplier_finder_helper prev_board new_loc new_dir new_acc else
    acc
  | Hor len ->
    if len > 0 then
      let new_loc = (fst loc, snd loc + 1)
      and new_dir = Hor (len - 1)
      and multiplier = snd (multiplier_at_loc prev_board loc) in
      let new_acc = acc * multiplier in
      word_multiplier_finder_helper prev_board new_loc new_dir new_acc else
    acc

let word_multiplier_finder prev_board loc dir = 
  word_multiplier_finder_helper prev_board loc dir 1

(**[word_value prev_board word] is the sum of letter values in the word [word], 
    using multipliers found on board [prev_board] *)
let word_value prev_board = function
| NoWord -> 0
| Word(word, loc, dir) -> 
  let word_multiplier = word_multiplier_finder prev_board loc dir in
  word_multiplier * (word_value_helper prev_board word loc dir 0)

(** [word_comparator word1 word2] is 
    1 if word1 is a word, and word2 is not a word
    0 if both words are NoWord or both words are a word 
    -1 if word1 is not a word, and word2 is a word. This will push all words
    to the front of an array, and NoWords to the end. *)
let word_comparator word1 word2 = match word1 with
| NoWord -> begin
  match word2 with
  | NoWord -> 0
  | Word(_, _, _) -> 1 end
| Word(_, _, _) -> begin
  match word2 with
  | NoWord -> -1
  | Word(_,_,_) -> 0 end

(**[append_new_words] appends all the new words in possible_new_words to 
  current_words. Requires that possible new words not already be in 
  current_words, and that the new words contain real words. *)
let append_new_words _ =
  Array.sort word_comparator possible_new_words;
  let num_of_new_words = find_next_index possible_new_words in
  let last_dst_index = find_next_index current_words in
  Array.blit possible_new_words 0 current_words last_dst_index num_of_new_words

(** [cur_and_possible_subarray cur_len pos_len] is the array conatingin all the
    words of [current_words] and [possible_new_words], without NoWord objects.
    Requires: At least one of cur_len or pos_len be greater than 0 (In other
    words, this should only be called once possible new words for this turn 
    been added to [possible_new_words]. ) *)
let cur_and_possible_subarray cur_len pos_len = 
  if cur_len > 0 then
    let cur_subarray = Array.sub current_words 0 cur_len in 
    if pos_len > 0 then
      let pos_subarray = Array.sub possible_new_words 0 pos_len in
      Array.append pos_subarray cur_subarray else
    cur_subarray else
  if pos_len > 0 then Array.sub possible_new_words 0 pos_len else
    failwith "No letters are on the board. (This should be impossible to reach)"

(** [build_board_cur_and_pos_words] is a function which returns a board, with 
    all the current and possible new words added onto it.
    Requires: Possible new words for this turn have already been added to the 
    array [possible_new_words].*)
let build_board_cur_and_pos_words () : t =
  let len_cur_words = find_next_index current_words
  and len_pos_words = find_next_index possible_new_words in
  let cur_and_pos_words = cur_and_possible_subarray len_cur_words len_pos_words
  and new_board = board_init () in
  for i=0 to len_cur_words + len_pos_words - 1 do
    let word_obj = cur_and_pos_words.(i) in
    place_word new_board word_obj
  done;
  new_board

(** [center_in_range start_row start_col end_row end_col] checks if the center 
    tile 7 7 is in rnage of start_row start_col end_row end_col.
    Requires: range length is greater than one. *)
let center_in_range start_row start_col end_row end_col =
  if start_row = 7 then
    if start_col <= 7 && end_col >= 7 then true else false else
  if start_col = 7 then
    if start_row <= 7 && end_row >= 7 then true else false else
  false

(** [touches_other_word] checks if the new word modifies any other word. 
    This is true if the original word modifies another word, and false 
    otherwise. *)
let touches_other_word () =
  let num_of_modified_words = find_next_index possible_new_words
  and num_of_tch_but_unmod = find_next_index words_touching_but_not_modified in
  let num_of_words = num_of_modified_words + num_of_tch_but_unmod in
  if num_of_words >= 2 then true else false

(** [is_original_word_touching board start_row start_col end_row end_col] is 
    true if the word in range of [start_row] [start_col] to [end_row] [end_col]
    is either touching the center tile, or is touching another word.
    Otherwise, return false. *)
let is_original_word_touching board start_row start_col end_row end_col =
  if center_in_range start_row start_col end_row end_col || 
    touches_other_word () then true else false

(** [check_for_floating_words board] is true if there are no floating letters 
    or words in [board], and false otherwise. *)
let check_for_floating_words board start_row start_col end_row end_col = 
  string_of_board board = string_of_board (build_board_cur_and_pos_words ()) &&
  is_original_word_touching board start_row start_col end_row end_col

(** [new_board_from_cur_words ()] returns a new board, with all the current 
    words placed on it. *)
let new_board_from_cur_words () : t =
  let new_board = board_init ()
  and num_of_words = find_next_index current_words in
  for i = 0 to num_of_words - 1 do
    let word_obj = current_words.(i) in
    place_word new_board word_obj
  done;
  new_board

(**[count_points] will count the value of all the new words created from this 
    play, and return it as an int option. Requires that all words in 
    possible_new_words are real, and created/modified in this turn *)
let count_points prev_board =
  append_new_words ();
  Some (possible_new_words |> Array.map (word_value prev_board) 
  |> Array.fold_left ( + ) 0)

(** [check_word_helper board start_row start_col end_row end_col] is an 
    int option, which is Some int if all the words created by writing the word 
    positioned at (start_row, start_col) to (end_row, end_col) are real, where
    the int is the number of points gained from the new words.
    [check_word_helper] is None if any of the modified words are non-real. 
    Raises: NoTileInCenter if there's no tile in location 7 7.
            InvalidStartOrEndPos if the word contains spaces, or if the end 
              position is further left or upwards or diagonal to the 
              start position. *)
let check_word_helper board start_row start_col end_row end_col =
  if is_empty board 7 7 then raise(NoTileInCenter) else
    Array.fill possible_new_words 0 7 NoWord;
    Array.fill words_touching_but_not_modified 0 19 NoWord;
    let prev_board = new_board_from_cur_words () in
    match length_and_dir start_row start_col end_row end_col with
    | Vert(len) -> 
      for i = 0 to (len - 1) do
        let row = start_row + i in
        let col = start_col in
        if is_empty board row col then raise(EmptySpace) else
        
        find_modified_words_vert board prev_board row col
      done;
      add_original_word board start_row start_col end_row end_col;
      if check_for_floating_words board start_row start_col end_row end_col then
        if are_words_real () then (count_points prev_board) else 
          raise(NonRealWord) else
      raise(FloatingLetter)
    | Hor(len) ->
      for i = 0 to (len - 1) do
        let row = start_row in
        let col = start_col + i in
        if is_empty board row col then raise(EmptySpace) else
        find_modified_words_hor board prev_board row col
      done;
      add_original_word board start_row start_col end_row end_col;
      if check_for_floating_words board start_row start_col end_row end_col then
        if are_words_real () then (count_points prev_board) else 
          raise(NonRealWord) else
      raise(FloatingLetter)

(** [exn_print exn] will print out to console why [exn] may have been raised*)
let exn_print exn = match exn with
| NoTileInCenter -> 
  print_newline ();
  print_string "Word rejected due to the center space being empty. The first 
  play must place a tile in the center space (7, 7). Try again.";
  print_newline ()
| InvalidStartOrEndPos -> 
  print_newline ();
  print_string "Word rejected due to the end position being further up, left, 
  or diagonal to the start position. Try again.";
  print_newline()
| EmptySpace -> 
  print_newline();
  print_string "Word rejected because the word either includes spaces, or there 
  was an empty space included in the spaces between your word's start position 
  and end position. Try again.";
  print_newline()
| Assert_failure (_, _, _) ->
  print_newline ();
  print_string "Word rejected because the starting or ending position was out of
   range of the board (0..14) x (0..14). Try again.";
  print_newline ()
| NonRealWord -> 
  print_newline ();
  print_string "Word rejected because this word, or one the words it modifies, 
  is not in the English dictionary. Try again. ";
  print_newline ();
| FloatingLetter ->
  print_newline ();
  print_string "Word rejected because there are floating letters on the board, 
  or the start and end position didn't capture the entire word you placed
  Try again. ";
  print_newline ();
| _ -> 
  print_newline(); print_string "An unkown error occurred."; print_newline()

(** [build_board_from_current_words board] will replace board with a board 
    consisting of only the verified valid words. *)
let build_board_from_current_words board =
  let num_of_words = find_next_index current_words in
  for i = 0 to num_of_words - 1 do
    let word_obj = current_words.(i) in
    place_word board word_obj
  done

let reset_board board = 
  let new_board = board_init () in
  Array.blit new_board 0 board 0 15;
  build_board_from_current_words board


 (** [check_word board start_row start_col end_row end_col] iterates over each 
  letter of the word. Check if it's empty. If it's empty, raise an exception. 
  If it isn't empty, check left. If the left is not empty. Iterate left until 
  the beginnning of the word. then iterate right until the end of the word, and 
  record the word. Repeat until last letter reached, and add the original word 
  as well. Then check that all words are real, and that they haven't been used 
  before. If they haven't been used, add the points. If all words are real, 
  store the word, direction, and starting location. If not 
  all words are real, remove new characters. *) 
let check_word board start_row start_col end_row end_col =
  try check_word_helper board start_row start_col end_row end_col with
  | NoTileInCenter -> 
    reset_board board;
    exn_print NoTileInCenter; 
    None
  | InvalidStartOrEndPos -> 
    reset_board board;
    exn_print InvalidStartOrEndPos; 
    None
  | EmptySpace -> 
    reset_board board;
    exn_print EmptySpace;
    None
  | Assert_failure(a, b, c) -> 
    reset_board board;
    exn_print (Assert_failure(a, b, c)); 
    None
  | NonRealWord -> 
    reset_board board; 
    exn_print (NonRealWord); 
    None
  | FloatingLetter ->
    reset_board board;
    exn_print (FloatingLetter);
    None