
(** Representation of a player's hand 

    This Module stores what tiles each player has in their hand*)

  (**Abstract value representing the hand *)
  type t
  
  (** [print_hor] prints out a horizontal visualization of the board. 
      This is used for the top and bottom player*)
  val print_hor : t -> unit

   (** [print_vert] prints out a vertical visualization of the board. 
      This is used for the right and left player*)
  val print_hor : t -> unit
