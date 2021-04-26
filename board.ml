(** [space] represents the value contained at a location. It is either Empty, or
  a Char *)
type space =
| Empty
| Char of char

exception UnknownPos

exception PosOccupied

exception EmptySpace

exception CharacterNotInAlphabet

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


(** [space_of_char chr] converts chr into an object of type space with 
    character [chr] *)
let space_of_char (chr : char) : space = Char(chr)

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
  Array.iter print_row board; print_newline();