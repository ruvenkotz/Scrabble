module Hand: sig

type tile

val index_r: int-> char

val generateHand: tile list -> tile list

val printHor: tile list -> unit

val printVert: tile list-> unit

end