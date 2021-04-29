open Yojson.Basic.Util

(** [space] represents the value contained at a location. It is either Empty, or
  a Char *)
type space =
| Empty
| Char of char

exception UnknownPos

exception PosOccupied

exception EmptySpace

exception CharacterNotInAlphabet

exception NoTileInCenter

exception InvalidStartOrEndPos

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

(** [char_of_space spce] converts an object of type space into a character. 
    Raises EmptySpace if [spce] is Empty. *)
let char_of_space (spce : space) : char = match spce with
| Empty -> raise(EmptySpace)
| Char(chr) -> chr

let is_empty board row col =
  if row > 14 || row < 0 || col > 14 || col < 0 then raise(UnknownPos) else
    let current_space = board.(row).(col) in
    match current_space with
    | Empty -> true
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
  | Empty -> print_string ("* "); 
  | Char(chr) -> print_string (Char.escaped chr ^ " "); in
  let print_row row = Array.iter print_space row; print_newline(); in
  Array.iter print_row board; print_newline()

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
    assert(end_col > start_col); Hor(end_col - start_col + 1) end else
  if (end_col - start_col = 0) then begin
    assert(end_row > start_row); Vert(end_row - start_row + 1) end else
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

(** [find_next_index_helper arr acc] iterates over arr until an empty space is 
    found, and returns the accumulator [acc]*)
let rec find_next_index_helper arr acc = match arr.(acc) with
| NoWord -> acc
| Word(_) -> find_next_index_helper arr (acc + 1)

(** [find_next_index arr] finds the next empty index in array [arr]. *)
let find_next_index arr = find_next_index_helper arr 0

(**[find_modified_words_vert board row col] finds all the horizontal 
    words which use the tile located at (row, col) *)
let find_modified_words_vert board row col : unit =
  let left = find_leftmost_nonempty_space board row col in
  let right = find_rightmost_nonempty_space board row col in
  if left = right then () else
    let new_word = 
      build_word board (fst left) (snd left) (fst right) (snd right) in
    let dir = length_and_dir (fst left) (snd left) (fst right) (snd right) in
    let word_and_loc  = Word(new_word, left, dir) in
    if Array.mem word_and_loc current_words then () else
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
let find_modified_words_hor board row col : unit =
  let top = find_topmost_nonempty_space board row col in
  let bottom = find_bottommost_nonempty_space board row col in
  if top = bottom then () else
    let new_word = 
      build_word board (fst top) (snd top) (fst bottom) (snd bottom) in
    let dir = length_and_dir (fst top) (snd top) (fst bottom) (snd bottom) in
    let word_and_loc  = Word(new_word, top, dir) in
    if Array.mem word_and_loc current_words then () else
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

(** [letter_value letter] returns the scrabble value of letter [letter]. *)
let letter_value letter =
  if letter = 'A' then 1 else
  if letter = 'B' then 3 else
  if letter = 'C' then 3 else
  if letter = 'D' then 2 else
  if letter = 'E' then 1 else
  if letter = 'F' then 4 else
  if letter = 'G' then 2 else
  if letter = 'H' then 4 else
  if letter = 'I' then 1 else
  if letter = 'J' then 8 else
  if letter = 'K' then 5 else
  if letter = 'L' then 1 else
  if letter = 'M' then 3 else
  if letter = 'N' then 1 else
  if letter = 'O' then 1 else
  if letter = 'P' then 3 else
  if letter = 'Q' then 10 else
  if letter = 'R' then 1 else
  if letter = 'S' then 1 else
  if letter = 'T' then 1 else
  if letter = 'U' then 1 else
  if letter = 'V' then 4 else
  if letter = 'W' then 4 else
  if letter = 'X' then 8 else
  if letter = 'Y' then 4 else
  10

(**[word_value_helper word acc] is the sum of letter values in the word so far. 
    Reminaing values to be counted are in string [word], and the accumulated 
    points so far are stored in int [acc].*)
let rec word_value_helper word acc =
  if String.length word = 0 then acc else
  let letter_val = letter_value word.[0] in
  let substring_len = String.length word - 1 in
  let substring = String.sub word 1 substring_len in
  word_value_helper substring (acc + letter_val)

(**[word_value word] is the sum of letter values in the word [word]. *)
let word_value = function
| NoWord -> 0
| Word(word, _, _) -> word_value_helper word 0

(**[string_of_word word] returns the string of the word object. 
    If the word object is NoWord, then the empty string is returned. *)
let string_of_word = function
| NoWord -> ""
| Word(word, _, _) -> word

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

(**[count_points] will count the value of all the new words created from this 
    play, and return it as an int option. Requires that all words in 
    possible_new_words are real, and created/modified in this turn *)
let count_points _ =
  append_new_words ();
  Some (possible_new_words |> Array.map word_value |> Array.fold_left ( + ) 0)

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
    match length_and_dir start_row start_col end_row end_col with
    | Vert(len) -> 
      for i = 0 to (len - 1) do
        let row = start_row + i in
        let col = start_col in
        if is_empty board row col then raise(InvalidStartOrEndPos) else
        find_modified_words_vert board row col
      done;
      add_original_word board start_row start_col end_row end_col;
      if are_words_real () then (count_points ()) else None
    | Hor(len) ->
      for i = 0 to (len - 1) do
        let row = start_row in
        let col = start_col + i in
        if is_empty board row col then raise(InvalidStartOrEndPos) else
        find_modified_words_hor board row col
      done;
      add_original_word board start_row start_col end_row end_col;
      if are_words_real () then (count_points ()) else None

let check_word board start_row start_col end_row end_col =
  try check_word_helper board start_row start_col end_row end_col with
  | NoTileInCenter -> 
    print_string ("Error: " ^ (Printexc.to_string NoTileInCenter)); None
  | InvalidStartOrEndPos -> 
    print_string ("Error: " ^ (Printexc.to_string InvalidStartOrEndPos)); None
  | EmptySpace -> 
    print_string ("Error: " ^ (Printexc.to_string EmptySpace)); None
  | Assert_failure(a, b, c) ->
    print_string ("Error: " ^ (Printexc.to_string (Assert_failure(a, b, c)))); None

(* iterate over each letter of the word. Check if it's empty. If it's empty, 
  raise an exception. If it isn't empty, check left. If the left is not empty. 
Iterate left until the beginnning of the word. then iterate right until the end 
of the word, and record the word. Repeat until last letter reached, and add the 
original word as well. Then check that all words are real, and that they haven't
 been used before. If they haven't been used, add the points. If all words are 
 real, store the word, direction, and starting location. If not all words are 
 real, remove new characters. *)

 (* add possible_new_word to current_words, and check for floating words*)