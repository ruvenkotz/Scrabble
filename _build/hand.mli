(** Representation of a player's hand 

    This Module stores what tiles each player has in their hand*)
  
  (** [generate_hand] uses the [next_tile] function from Bag to add seven tiles 
into a player's hand*)
  val generate_hand : Bag.tile list -> Bag.tile list
  (** [print_hor] prints out a horizontal visualization of the board. 
      This is used for the top and bottom player*)
  val print_hor : Bag.tile list -> unit

   (** [print_vert] prints out a vertical visualization of the board. 
      This is used for the right and left player*)
  val print_vert : Bag.tile list-> unit

  