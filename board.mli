(** Representation of static board data.

    This module stores what letter is stored in what location. *)


(** [space] represents the value contained at a location. It is either Empty, or
  a Char *)
  type space = 
  | Empty
  | Char of char

type row = space array

(** The abstract type t represents a board. *)
type t = row array

(** Raised when location called outside of board's range *)
exception UnknownPos

(** Raised when a letter is already located at this position *)
exception PosOccupied

(** Raised when trying to convert an empty space into a character *)
exception EmptySpace

(** Raised when attempting to set a character, but the character isn't within 
    A-Z or a-z. *)
exception CharacterNotInAlphabet

(** [is_Empty board row col] is true if no character is stored at [row] [col], 
    and false otherwise.
    Raises UnknownPos if [row] or [col] is not wihtin the range of 0 and 14.  *)
val is_empty : t -> int -> int -> bool

(** [get_char board row col] is the character located at [row] [col].
    (The leftmost column is column 0, and increases as we move to the right. 
    The topmost row is row 0, and the row number increases as we move down)
    Requires: row and col both be be within 0 and 14. 
    Raises: UnknownPos if either [row] or [col] is not within 0 and 14. 
            EmptySpace if no character is stored at [row] [col] *)
val get_char : t -> int -> int -> char

(** [set_char board row col chr] replaces the space at [row] [col] of [board]
    with [chr]. 
    Requires: [row] and [col] are both wihtin 0 and 14. 
    No letter currently occupies [row] [col]
    Raises: UnknownPos if the either [row] or [col] is not within 0 and 14. 
            PosOccupied if a letter is already in [row] [col]
            CharacterNotInAlphabet if the character is not in A-Z or a-z.  *)
val set_char : t -> int -> int -> char -> unit

(** [print_board board] prints [board] to console *)
val print_board : t -> unit

(** [check_word board start_row start_col end_row end_col] checks that the word
    placed from (start_row, start_col) to (end_row, end_col) is a real word, and
    all other words affected by this word are also real words. If all words are
    real, then Some int is returned, where that int is the points gained from
    all the *new* words formed. If any word formed is not accpeted, then 
    [check_word] returns None. If None is returned, the board is reset to the 
    state it was in before this play (aka new tiles placed in this turn are 
    erased).
    
    Example: We have board:
                * * * * *
                * * C * *
                * * A * *
                * * T * *
                * * * * *
            If a player places the word "CAR", such that the new board is:
                * * * * *
                * * C * *
                * C A R *
                * * T * *
                * * * * *
            We would call [check_word board 1 2 3 2], since "CAR" start at 
            position (1, 2) and ends at position (3, 2).

            [check_word] would then return Some 5, because "CAR" and "CAT" are 
            both real words, and the value of "CAR" is 5. "CAT" doesn't add to 
            the points, because the original word "CAT" was not changed by this 
            play.

            If a player places the word "ICE" such that the new board is:
                * * * * *
                * I C E *
                * C A R *
                * * T * *
                * * * * *
            [check_word board 1 1 1 3] would be Some 11, because placing ICE
            also formed the words IC and ER. ICE is worth 5, IC is worth 4, and
            ER is worth 2, for a total of 11.contents

            If a player places the word ZICU, such that the board is:
                * Z * * *
                * I C E *
                * C A R *
                * U T * *
                * * * * *
            [check_word board 0 1 3 1] wold return None, and the board would be
            reset to:
                * * * * *
                * I C E *
                * C A R *
                * * T * *
                * * * * *
            because ZICU is not a word in the English dictionary.
    *)
val check_word : t -> int -> int -> int -> int -> int option