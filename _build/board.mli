(** Representation of static board data.

    This module stores what letter is stored in what location. *)


(** The abstract type t represents a board. *)
type t

(** Raised when location called outside of board's range *)
exception UnknownPos

(** Raised when a letter is already located at this position *)
exception PosOccupied

(** Raised when trying to convert an empty space into a character *)
exception EmptySpace

(** Raised when attempting to set a character, but the character isn't within 
    A-Z or a-z. *)
exception CharacterNotInAlphabet

(** [board_init] is a 15 x 15 board, with all spaces empty *)
val board_init : t

(** [is_Empty board row col] is true if no character is stored at [row] [col], 
    and false otherwise.
    Raises UnknownPos if [row] or [col] is not wihtin the range o 0 and 14.  *)
val is_empty : t -> int -> int -> bool

(** [get_char board row col] is the character located at [row] [col].
    (The leftmost column is column 0, and increases as we move to the right. 
    The topmost row is row 0, and the row number increases as we move down)
    Requires: row and col both be be within 0 and 14. 
    Raises: UnknownPos if either [row] or [col] is not within 0 and 14. 
            EmptySpace if no character is stored at [row] [col] *)
val get_char : t -> int -> int -> char

(** [set_char board row col chr] returns a new board, based on [board], but with
    [chr] placed at [row] [col]. 
    Requires: [row] and [col] are both wihtin 0 and 14. 
    No letter currently occupies [row] [col]
    Raises: UnknownPos if the either [row] or [col] is not within 0 and 14. 
            PosOccupied if a letter is already in [row] [col]
            CharacterNotInAlphabet if the character is not in A-Z or a-z.  *)
val set_char : t -> int -> int -> char -> t

(** [print_board board] prints [board] to console *)
val print_board : t -> unit