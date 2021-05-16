(**Representation of the bag of tiles 

This module represents the dynamic bag of tiles in a scrabble game. It is 
updated whenever a player places tiles or tries to exchange tiles*) 

(**Abstract value representing the bag*)
type t

(**Abstract value representing a tile*)
type tile = {
  letter : char;
  value : int;
}

(**Raised when there is no tiles left in the bag*)
exception EmptyBag

(**Raised when the tile is not present in the bag*)
exception TileNotFound

(***Raised when the charactr is not between A-Z*)
exception InvalidChar

(**[init_bag] makes the initial bag with 102 tiles, with the appropriate counts 
for them.*)
val init_bag: t

(**[next_tile] prints out the value of the tile drawn, and then returns a 
    mutated bag with an updated amount of tiles.
    Raises: *)
val next_tile: t-> (t * tile)



(**[tile_value] is the value associated in scrabble with a character of a 
given value*)
val tile_value: char -> int 

(** [total_count] is the amount of tiles with given char in the bag *)
val total_count: t -> int

(**[return_tile] allows a tile to be returned to the bag.*)
val return_tile: tile->t->unit

