(** [space] represents the value contained at a location. It is either Empty, or
  a Char *)
type space =
| Empty
| Char of char

exception UnknownPos

exception PosOccupied

exception EmptySpace

(** Raised when attemptng to access an index of a list outside of the list's 
    size*)
exception IndexOutOfBounds

(** [row] is a list a spaces, organized from left to right. 
Example: The row:
          A B C D E F 
          Would be represented as:
          ['A';'B';'C';'D';'E';'F']          
Requires: Must be 15 spaces long. *)
type row = space list

(** [t] represents the board itself, and it consists of a list of rows. 
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
type t = row list

let get_char (board : t) (row : int) (col : int) : space =
  if row > 14 || row < 0 || col > 14 || col < 0 then raise(UnknownPos) else
    let current_row = List.nth board row in
    List.nth current_row col

(** [updated_list_helper left_lst right_lst index chr] iterates over the 
    right_lst until we reach the index we're looking for.
    Note: left_lst is in the reverse order
    
    If we're at the index we're looking for, we append the reverse of the 
    left_lst to the right_lst, but with the first space replaced with [chr]. 
    Otherwise, we move the head of the right_lst to the head of the left_lst, 
    and call update_list_helper on the new left_lst, the reminaing right_lst, 
    and with one index less
    
    Raises: PosOccupied if the space we're trying to replace is not Empty *)
let rec update_space_helper left_lst right_lst index chr =
  if index = 0 then
    match List.hd right_lst with
    | Char(_) -> raise(PosOccupied)
    | Empty -> List.rev left_lst @ (chr :: List.tl right_lst) 
  else
    update_space_helper (List.hd right_lst :: left_lst) (List.tl right_lst) 
      (index - 1) chr

(** [update_space lst index chr] returns a list with the space at [index] 
    replaced with chr.
    Raises: IndexOutOfBounds if index is not between 0 and 14 inclusive. *)
let update_space lst index chr = 
  if index < 0 || index > 14 then raise(IndexOutOfBounds) else
    update_space_helper [] lst index chr
  
(** [update_row_helper left_lst right_lst index new_row] will iterate over the 
    right_lst until we reach the desired index. 
    Note: The left_lst is in reverse order, to keep time-complexity low.
    
    If we are at the desrired index, we append the reverse of the left_lst with 
    the new row concatenated onto the tail of the right_lst.
    If we're not at the desired index, we call update_row_helper, with the head 
    of the right_lst concatenated onto the left_lst, and the rest of the 
    right_lst *)
let rec update_row_helper (left_lst : row list) (right_lst : row list) 
  (index : int) (new_row : row) : row list =
  if index = 0 then 
      (List.rev left_lst) @ (new_row :: List.tl right_lst) 
  else
    update_row_helper (List.hd right_lst :: left_lst) (List.tl right_lst) 
      (index - 1) new_row

(** [update_row lst index row] returns a new board with the row at index [index]
   replaced with [new_row].
   Raises: IndexOutOfBounds if [index] is less than 0 or greater than 14. *)
let update_row (board : t) (index : int) (new_row : row) : t = 
  if index < 0 || index > 14 then raise(IndexOutOfBounds) else
    update_row_helper [] board index new_row

(** [space_of_char chr] converts chr into an object of type space with 
    character [chr] *)
let space_of_char (chr : char) : space = Char(chr)

(** [char_of_space spce] converts an object of type space into a character. 
    Raises EmptySpace if [spce] is Empty. *)
let char_of_space (spce : space) : char = match spce with
| Empty -> raise(EmptySpace)
| Char(chr) -> chr

let set_char (board : t) (row : int) (col: int) (chr : char) : t =
  if row > 14 || row < 0 || col > 14 || col < 0 then raise(UnknownPos) else
    let current_row = List.nth board row in
    let new_row = update_space current_row col (space_of_char chr) in
    update_row board row new_row