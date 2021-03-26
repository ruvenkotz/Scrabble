(** Representation of static board data.

    This module stores what letter is stored in what location. *)


(** The abstract type t represents a board. *)
type t

(** The type containing what is in a space *)
type space = 
| Empty
| Char of char

(** Raised when location called outside of board's range *)
exception UnknownPos

(** Raised when a letter is already located at this position *)
exception PosOccupied

(** Raised when trying to convert an empty space into a character *)
exception EmptySpace

(** [get_char board row col] is the character located at [row] [col].
    (The leftmost column is column 0, and increases as we move to the right. 
    The topmost row is row 0, and the row number increases as we move down)
    Requires: row and col both be be within 0 and 14. 
    Raises: UnknownPos if either [row] or [col] is not within 0 and 14. *)
val get_char : t -> int -> int -> space

(** [set_char board row col chr] returns a new board, based on [board], but with
    [chr] placed at [row] [col]. 
    Requires: [row] and [col] are both wihtin 0 and 14. 
    No letter currently occupies [row] [col]
    Raises: UnknownPos if the either [row] or [col] is not within 0 and 14. 
    PosOccupied if a letter is already in [row] [col]] *)
val set_char : t -> int -> int -> char -> t

(** [char_of_space space] converts a space into its character.
    Raises: EmptySpace if the space is Empty*)
val char_of_space : space -> char