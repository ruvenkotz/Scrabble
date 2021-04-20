open Bag
open Board
(** Representation of a player's hand 

    This Module stores what tiles each player has in their hand*)
  
  (*Abstract value representing a hands*)
  type t = Bag.tile list list

  (**Raised when there s player chooses a letter that isn't in their hand*)
  exception LetterNotFound
  (**Added this to mli so Ryan could access in main*)
  val generate_hand_helper : Bag.tile list -> Bag.tile list
  (** [init_hand] uses the [next_tile] function from Bag to add seven tiles 
into a player's hand*)
  val init_hand : Bag.tile list

  val add_a_hand : t
  (** [print_hor] prints out a horizontal visualization of the board. 
      This is used for the top and bottom player*)
  val print_hor : Bag.tile list -> unit

  (* [play_a_word] plays a word from a player's hand onto the board*)
  val play_a_word : Board.t-> Bag.tile list -> tile list  -> Board.t

   (** [print_vert] prints out a vertical visualization of the board. 
      This is used for the right and left player*)
  val print_vert : Bag.tile list -> unit

  