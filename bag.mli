(**Representation of the bag of tiles 

This module represents the dynamic bag of tiles in a scrabble game. It is 
updated whenever a player places tiles or tries to exchange tiles*) 

(*Abstract value representing the bag*)
type t

type tile = {
  letter : string;
  value : int;
}

(*Prints out the value of the tile drawn, and then returns a mutated bag with 
an updated amount of tiles.*)
val next_tile: t-> tile

