(** Representation of a player's hand 

This Module stores what tiles each player has in their hand*)

(*Abstract value representing a hands*)
type t = Bag.tile array 

(**Raised when a player chooses a letter that isn't in their hand*)
exception LetterNotFound

(**Raised when a player chooses a position not on the board 
or one that's occupied *)
exception InvalidPositioning

(*[create_starting_hand]  uses the [next_tile] function from Bag to add 
seven tiles into a player's hand*)
val create_starting_hand : t-> Bag.t-> unit

(** [print_hor] prints out a horizontal visualization of the board. 
This is used for the top and bottom player*)
val print_hor : t-> unit

(** [print_vert] prints out a vertical visualization of the board. 
This is used for the right and left player*)
val print_vert : t -> unit

(** [tile_getter] Returns the tile with the letter value letter in hand. 
Throws [LetterNotFound] if not*)
val tile_getter: char-> t -> int -> Bag.tile

(** [tile_replace] replaces the given tile with one that is randomly 
drawn from the bag*)
val tile_replace: Bag.tile -> t -> Bag.t -> unit

(* [play_a_word] plays tiles from a player's hand to form a word and calls 
   [check word]. If all the words formed by the new word are valid then it 
   will return Some int corresponding to the points scored by that new word.
   If it's all the words aren't valid it will return None. If the play tries
   to play a tile that is not in their hand, it will raise LetterNotFound. If 
   the player tries to choose a space that isn't on the board it will raise
   Unknown Pos. If a player tries to choose a space that is occupied, it will 
   raise PosOccupied*)
val play_a_word : Board.t-> t -> Bag.tile list ref ->  int option

val place_a_letter : Board.t -> string -> string -> Bag.tile list -> unit

(**[revert_hand] adds back the original tiles that were placed
in case of failure*)
val revert_hand: t -> Bag.tile list-> unit